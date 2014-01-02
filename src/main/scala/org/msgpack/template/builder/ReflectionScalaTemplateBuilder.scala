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
import java.lang.Class

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

  private def toScalaTemplates(entries: Array[FieldEntry]): Array[ReflectionScalaFieldTemplate[AnyRef]] = {
    entries.map {
      e =>
        if (e.isAvailable) {
          val template = registry.lookup(e.getGenericType).asInstanceOf[Template[AnyRef]]
          new ReflectionScalaFieldTemplate(e.asInstanceOf[ScalaFieldEntry], template)
        } else {
          null
        }
    }
  }
}

/**
 * Store companion object
 */
object CompanionObjectMap {

  type CompanionObject = {def apply(): Any}

  var companions = Map[Class[_], CompanionObject]()

  def newInstance(clazz: Class[_]) = companions.synchronized {
    if (companions.contains(clazz)) {
      companions(clazz).apply()
    } else {
      try {
        clazz.newInstance
      } catch {
        case e: InstantiationException => {
          val c = registerCompanionObject(clazz)
          c.apply()
        }
      }
    }
  }

  def registerCompanionObject(clazz: Class[_]): CompanionObject = {
    try {
      val companion = clazz.getClassLoader.loadClass(clazz.getName + "$").getDeclaredField("MODULE$").get(null).asInstanceOf[CompanionObject]
      companions += (clazz -> companion)
      companion
    } catch {
      case e: ClassNotFoundException => {
        throw new MessageTypeException("Can't find plain constructor or companion object")
      }
      case e: NoSuchFieldException => {
        throw new MessageTypeException("Can't find plain constructor or companion object")
      }
    }
  }

}

class ReflectionScalaTemplate[T <: AnyRef](var targetClass: Class[T],
                                           var templates: Array[ReflectionScalaFieldTemplate[T]]) extends AbstractTemplate[T] {
  def read(unpacker: Unpacker, base: T, required: Boolean): T = {
    if (!required && unpacker.trySkipNil) {
      return null.asInstanceOf[T]
    }
    val to: T = if (base == null) {
      CompanionObjectMap.newInstance(targetClass).asInstanceOf[T]
    } else base
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
