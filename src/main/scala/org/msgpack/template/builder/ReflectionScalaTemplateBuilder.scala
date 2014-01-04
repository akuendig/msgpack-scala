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
 * Cache for object constructors.
 */
object ConstructorMap {

  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}
  import scala.language.reflectiveCalls

  type Constructor = (() => Any)

  // This is a static "constructor" we store when we cannot find a constructor
  private val throwingConstructor =
    () => throw new MessageTypeException("Can't find plain constructor or companion object")

  // Lock and cache of constructors
  private val constructorsLock = new Object()
  private var constructors = Map.empty[Class[_], Constructor]

  def newInstance[T](clazz: Class[T]): T = {
    val ctor = getOrCreate(clazz)

    ctor().asInstanceOf[T]
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

    val emptyConstructorOpt = emptyConstructor(clazz, classType)

    if (emptyConstructorOpt.isDefined)
      return emptyConstructorOpt.get

    // If there is no companion, we stop here, as there is no apply method
    // and no default arguments for the constructor.
    if (!hasCompanion(classSymbol))
      throwingConstructor

    // Get to the companion symbol via reflection
    val companionSymbol = classSymbol.companionSymbol.asModule
    val companionType = companionSymbol.typeSignature

    val constructorOrApply =
      constructorWithDefaultArgs(companionSymbol, companionType, classType).
        orElse(emptyApply(companionSymbol, companionType)).
        orElse(applyWithDefaultArgs(companionSymbol, companionType, classType))

    constructorOrApply.getOrElse(throwingConstructor)
  }

  private def emptyConstructor(clazz: Class[_], classType: Type): Option[Constructor] = {
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
    if (hasEmptyConstructor) {
      // Help type inference to not call the damn method...
      val ctor: Constructor = clazz.newInstance
      Some(ctor)
    } else {
      None
    }
  }

  private def getDefaultArgumentNames(m: MethodSymbol) = {
    //http://stackoverflow.com/questions/16939511/instantiating-a-case-class-with-default-args-via-reflection
    //    import scala.reflect.runtime.universe
    //    import scala.reflect.internal.{ Definitions, SymbolTable, StdNames }
    //
    //    val ds = universe.asInstanceOf[Definitions with SymbolTable with StdNames]

    // Constructor names are actually the name of the class
    // but their default argument getters start with <init> as of 2.11.0-M7
    val prefix =
      if (m.isConstructor) nme.CONSTRUCTOR
      else m.name.encoded

    for {
    // Take all arguments of the first argument list and index them
      (argument, index) <- m.paramss.head.map(_.asTerm).zipWithIndex

      // Filter out non-default parameters
      if argument.isParamWithDefault

    // Return a term name using string concatenation. API for getting a default arguments
    // getter name is not jet public as of 2.11.0-M7
    } yield TermName(prefix + "$default$" + (index+1))
  }

  private def constructorWithDefaultArgs(companionSymbol: ModuleSymbol, companionType: Type, classType: Type): Option[Constructor] = {
    // Start with all possible constructors
    // Only one constructor can define default arguments
    // see class A(var a: Int = 42, var b: Int = 12) { def this(aa: Int, bb: String = "Hello") = this(1, 2) }
    val constructors = classType.declarations.collect {
      case d if d.isMethod && d.asMethod.isConstructor => d.asMethod
    }

    // Obtain a mirror for the companion and the class
    val classMirror = cm.reflectClass(classType.typeSymbol.asClass)
    val companionMirror = cm.reflect(cm.reflectModule(companionSymbol).instance)

    val possibleConstructors = for {
      ctor <- constructors

      // Check that there is only one parameter list, as we can only handle that
      if ctor.paramss.size == 1

      // Get the names of all default argument getters
      paramsWithDefault = getDefaultArgumentNames(ctor)

      // Cache the declarations for the later search
      companionDeclarations = companionType.declarations

      // Get the method symbols by searching them in all declarations. Type.declaration(TermName)
      // does not work, probably because we construct the term name by hand. List.find returns
      // an Option, so we flatMap the result to get a plain list and not a List[Option].
      defaultGetters = paramsWithDefault.flatMap {
        p => companionDeclarations.find(_.name.encoded == paramsWithDefault.head.encoded)
      }

      // Only proceede if all arguments have a default
      if defaultGetters.size == ctor.paramss.head.size

      // Reflect the constructor and the default argument getters
      reflectedCtor = classMirror.reflectConstructor(ctor)
      reflectedGetters = defaultGetters.map(dg => companionMirror.reflectMethod(dg.asMethod))
    } // Yield a method, that evaluates the default getters and calls the constructor with the results
    yield () => {
        // Getter must always be lazy evaluated
        val defaults = reflectedGetters.map(_.apply())

        reflectedCtor.apply(defaults: _*)
      }

    // Take the first constructor, there should be at most one
    possibleConstructors.headOption
  }

  private def emptyApply(companionSymbol: ModuleSymbol, companionType: Type): Option[Constructor] = {
    companionType.declarations.collectFirst {
      case d
        if d.isMethod &&
          d.asMethod.paramss == List(List()) &&
          d.asMethod.name.decoded == "apply" =>

        val apply = d.asMethod

        // Obtain a mirror for the companion
        val companionMirror = cm.reflect(cm.reflectModule(companionSymbol).instance)

        val reflectedApply = companionMirror.reflectMethod(apply)

        () => reflectedApply.apply()
    }
  }

  private def applyWithDefaultArgs(companionSymbol: ModuleSymbol, companionType: Type, classType: Type): Option[Constructor] = {
    // Start with all possible apply methods
    // Only one apply can define default arguments.
    val applies = companionType.declarations.collect {
      case d if d.isMethod && d.asMethod.name.decoded == "apply" => d.asMethod
    }

    // Obtain a mirror for the companion
    val companionMirror = cm.reflect(cm.reflectModule(companionSymbol).instance)

    val possibleApplies = for {
      apply <- applies

      // Check that there is only one parameter list, as we can only handle that
      if apply.paramss.size == 1

      // Get the names of all default argument getters
      paramsWithDefault = getDefaultArgumentNames(apply)

      // Cache the declarations for the later search
      companionDeclarations = companionType.declarations

      // Get the method symbols by searching them in all declarations. Type.declaration(TermName)
      // does not work, probably because we construct the term name by hand. List.find returns
      // an Option, so we flatMap the result to get a plain list and not a List[Option].
      defaultGetters = paramsWithDefault.flatMap {
        p => companionDeclarations.find(_.name.encoded == paramsWithDefault.head.encoded)
      }

      // Only proceede if all arguments have a default
      if defaultGetters.size == apply.paramss.head.size

      // Reflect the apply method and the default argument getters
      reflectedApply = companionMirror.reflectMethod(apply)
      reflectedGetters = defaultGetters.map(dg => companionMirror.reflectMethod(dg.asMethod))
    } // Yield a method, that evaluates the default getters and calls the constructor with the results
    yield () => {
        // Getter must always be lazy evaluated
        val defaults = reflectedGetters.map(_.apply())

        reflectedApply.apply(defaults: _*)
      }

    // Take the first apply method, there should be at most one
    possibleApplies.headOption
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
        ConstructorMap.newInstance(targetClass)
      else
        base

    unpacker.readArrayBegin()

    for (template <- templates) {
      if (!template.entry.isAvailable) {
        unpacker.skip()
      } else if (template.entry.isOptional && unpacker.trySkipNil) {
        println("Skipped !" + template)
      } else {
        template.asInstanceOf[Template[T]].read(unpacker, to, false)
      }
    }

    unpacker.readArrayEnd()

    to
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

    v.asInstanceOf[T]
  }

  def write(pk: Packer, v: T, required: Boolean) = {
    template.write(pk, v, required)
  }

  override def toString = {
    "ReflectionScalaFieldTemplate for " + template
  }
}
