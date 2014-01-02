//
// MessagePack for Scala
//
// Copyright (C) 2009-2011 FURUHASHI Sadayuki
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//
package org.msgpack.template.builder

import org.msgpack.unpacker.Unpacker
import org.msgpack.packer.Packer
import org.msgpack.template.{AbstractTemplate, TemplateRegistry, Template}
import org.msgpack.MessageTypeException

/**
 *
 * User: takeshita
 * Create: 11/10/13 11:24
 */

class ReflectionScalaTemplateBuilder(registry: TemplateRegistry)
  extends ReflectionTemplateBuilder(registry) with ScalaObjectMatcher with ScalaPropertyFinder {

  override def buildTemplate[T](targetClass: Class[T], entries: Array[FieldEntry]) = {
    if (entries == null) {
      throw new NullPointerException("entries is null: " + targetClass)
    }

    val templates = toScalaTemplates(entries)
    new ReflectionScalaTemplate[AnyRef](targetClass.asInstanceOf[Class[AnyRef]], templates).asInstanceOf[Template[T]]
  }

  private def toScalaTemplates(entries: Array[FieldEntry]): Array[ReflectionScalaFieldTemplate[AnyRef]] =
    entries.collect {
      case e if e.isAvailable =>
        val template = registry.lookup(e.getGenericType).asInstanceOf[Template[AnyRef]]
        new ReflectionScalaFieldTemplate(e.asInstanceOf[ScalaFieldEntry], template)
    }
}

/**
 * Store companion object
 */
object CompanionObjectMap {
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{ currentMirror => cm }
  import scala.language.reflectiveCalls

  type Constructor = (() => Any)

  // This is a static "constructor" we store when we cannot find a constructor
  private val throwingConstructor =
    () => throw new MessageTypeException("Can't find plain constructor or companion object")

  // Lock and cache of constructors
  private val constructorsLock = new Object()
  private var constructors = Map.empty[Class[_], Constructor]

  def newInstance(clazz: Class[_]): Any = {
    val ctor = getOrCreate(clazz)

    ctor()
  }

  // Threadsafe method to lookup or write a new entry to the constructor cache
  private def getOrCreate(clazz: Class[_]): Constructor = constructorsLock.synchronized {
    if (constructors.contains(clazz))
      constructors(clazz)
    else {
      val constructor = createConstructor(clazz)

      constructors += clazz -> constructor

      constructor
    }
  }

  // Helper to check for companion
  private def hasCompanion(cls: ClassSymbol): Boolean =
    cls.companionSymbol != NoSymbol

  // Create a constructor either from a direct constructor or
  // the companion objects's apply method.
  private def createConstructor(clazz: Class[_]): Constructor = {
    // Enter Scala reflection world
    val classSymbol = cm.classSymbol(clazz)
    val classType = classSymbol.typeSignature

    // It is a constructor, if it is a method, a constructor, and
    // takes no parameters
    val hasEmptyConstructor = classType.declarations.exists {
      d =>
        d.isMethod &&
        d.asMethod.isConstructor &&
        d.asMethod.paramss == List(List())
    }

    // We just return the java constructor method. This is
    // cleaner than making the full reflective call via Scala.
    if (hasEmptyConstructor)
      return clazz.newInstance

    // If there is no companion, we stop here.
    if (!hasCompanion(classSymbol))
      throwingConstructor

    // This type is used to make a reflective call to an empty apply() method.
    type EmptyApply = { def apply(): Any }

    // Get to the companion symbol via reflection
    val companionSymbol = classSymbol.companionSymbol.asModule
    val companionInstance = cm.reflectModule(companionSymbol).instance

    // Use the structural type to check, if there is an empty apply() method.
    companionInstance match {
      case withApply: EmptyApply => withApply.apply
      case _ => throwingConstructor
    }
  }
}

class ReflectionScalaTemplate[T <: AnyRef](var targetClass: Class[T],
                                           var templates: Array[ReflectionScalaFieldTemplate[T]]) extends AbstractTemplate[T] {
  def read(unpacker: Unpacker, base: T, required: Boolean): T = {
    if (!required && unpacker.trySkipNil) {
      return null.asInstanceOf[T]
    }

    val to: T =
      if (base == null)
        CompanionObjectMap.newInstance(targetClass).asInstanceOf[T]
      else
        base

    unpacker.readArrayBegin
    var i: Int = 0
    while (i < templates.length) {
      {
        var tmpl = templates(i)
        if (!tmpl.entry.isAvailable) {
          unpacker.skip
        }
        else if (tmpl.entry.isOptional && unpacker.trySkipNil) {
          println("Skipped !" + tmpl)
        }
        else {
          tmpl.asInstanceOf[Template[T]].read(unpacker, to, false)
        }
      }
      ({
        i += 1;
        i
      })
    }
    unpacker.readArrayEnd
    return to
  }


  def write(packer: Packer, target: T, required: Boolean): Unit = {
    if (target == null) {
      if (required) {
        throw new MessageTypeException("attempted to write null")
      }
      packer.writeNil
      return
    }

    packer.writeArrayBegin(templates.length)
    for (template <- templates) {
      if (!template.entry.isAvailable) {
        packer.writeNil
      } else {
        val obj = template.entry.get(target).asInstanceOf[T]
        if (obj == null) {
          if (template.entry.isNotNullable) {
            throw new MessageTypeException(template.entry.getName + " cannot be null by @NotNullable")
          }
          packer.writeNil()
        } else {
          template.asInstanceOf[Template[T]].write(packer, obj, true)
        }
      }
    }
    packer.writeArrayEnd

  }
}

class ReflectionScalaFieldTemplate[T <: AnyRef](val entry: ScalaFieldEntry, template: Template[T]) extends AbstractTemplate[T] {
  def read(u: Unpacker, to: T, required: Boolean): T = {
    val f = entry.get(to).asInstanceOf[T]
    val v = template.read(u, f, required)
    if (v != f) {
      entry.set(to, v)
    }
    return v.asInstanceOf[T]
  }

  def write(pk: Packer, v: T, required: Boolean) = {
    template.write(pk, v, required)
  }

  override def toString = {
    "ReflectionScalaFieldTemplate for " + template
  }
}
