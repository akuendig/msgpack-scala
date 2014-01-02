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

import org.msgpack.template.FieldOption
import org.msgpack.annotation.{NotNullable, Optional, Index, Ignore}
import java.lang.annotation.{ Annotation => JAnnotation}
import java.lang.reflect.{Modifier, Field, Method, Type => JType, ParameterizedType}
import org.msgpack.scalautil.ScalaSigUtil
import org.msgpack.scalautil.ScalaSigUtil.Property
import scala.reflect.runtime.universe._
import org.slf4j.LoggerFactory

/**
 * Combination with java reflection and scalap.ScalaSigParser
 * User: takeshita
 * Create: 11/10/13 11:53
 */

trait ScalaPropertyFinder{
  self : AbstractTemplateBuilder =>

  val logger = LoggerFactory.getLogger(getClass)

  override def toFieldEntries(targetClass: Class[_], from: FieldOption) : Array[FieldEntry] = {
    val props = ScalaSigUtil.getAllProperties(targetClass)
    val indexed = indexing(props)

    indexed.map(convertToScalaFieldEntry)
  }

  def indexing(props: Seq[Property]): Array[Property] = {
    val indexed = new Array[Property](props.size)

    var notIndexed: List[Property] = Nil

    for (s <- props) {
      getAnnotation[Index](s) match {
        case None => notIndexed = notIndexed :+ s
        case Some(indexAnnotation) =>
          val index = ScalaSigUtil.
            annotationArg(indexAnnotation, "value").
            get.
            value.
            asInstanceOf[java.lang.Integer]

          if (indexed(index) != null) {
            throw new TemplateBuildException("duplicated index: " + index)
          } else if (index < 0 || index >= indexed.length) {
            throw new TemplateBuildException("invalid index: %s index must be 0 <= x < %s".format(index, indexed.length))
          } else {
            indexed(index) = s
          }
      }
    }

    for (i <- 0 until indexed.length) {
      if (indexed(i) == null) {
        indexed(i) = notIndexed.head
        notIndexed = notIndexed.drop(1)
      }
    }

    indexed
  }

  def getAnnotations(prop: Property): List[Annotation] = {
    val Property(_, getter, setter, _) = prop

    val scalaFieldAnnotations =
      if (getter.isGetter)
        setter.accessed.asTerm.annotations
      else
        Nil

    getter.annotations ++ scalaFieldAnnotations ++ setter.annotations
  }

  def hasAnnotation[T <: JAnnotation : TypeTag](prop: Property): Boolean = {
    getAnnotations(prop).
      exists(_.tpe =:= typeTag[T].tpe)
  }

  def getAnnotation[T <: JAnnotation : TypeTag](prop: Property): Option[Annotation] = {
    getAnnotations(prop).
      find(_.tpe =:= typeTag[T].tpe)
  }

  def convertToScalaFieldEntry(propInfo: Property) = {
    val Property(name, getter, setter, _) = propInfo

//    println(s"$name: ${getter.returnType} | ${getter.returnType.typeSymbol.fullName} | ${ScalaSigUtil.toErasedJavaClass(getter.returnType).getName}")

    getter.returnType match{
      case pt : ParameterizedType =>
        new ScalaFieldEntry(name,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          ScalaSigUtil.toErasedJavaClass(getter.returnType),
          ScalaSigUtil.toJavaClass(getter.returnType),
          ScalaSigUtil.toJavaMethod(getter),
          ScalaSigUtil.toJavaMethod(setter)
        )
      case t if t.typeSymbol.fullName == "scala.Enumeration.Value" =>
        new ScalaFieldEntry(name,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          ScalaSigUtil.toErasedJavaClass(getter.returnType),
          ScalaSigUtil.getCompanionObjectClass(getter.returnType).
            map(ScalaSigUtil.toJavaClass).
            get,
          ScalaSigUtil.toJavaMethod(getter),
          ScalaSigUtil.toJavaMethod(setter)
        )
      case t =>
        new ScalaFieldEntry(name,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          ScalaSigUtil.toErasedJavaClass(getter.returnType),
          ScalaSigUtil.toJavaClass(t),
          ScalaSigUtil.toJavaMethod(getter),
          ScalaSigUtil.toJavaMethod(setter)
        )
    }
  }

  def readValueType(prop: Property) = {
    prop.getter.returnType.typeSymbol.asClass
  }

  def readFieldOption(prop: Property, implicitOption: FieldOption) = {
    if (hasAnnotation[Optional](prop)) {
      FieldOption.OPTIONAL
    } else if (hasAnnotation[NotNullable](prop)) {
      FieldOption.NOTNULLABLE
    } else if (hasAnnotation[Ignore](prop)) {
      FieldOption.IGNORE
    } else {
      if (readValueType(prop).isPrimitive) {
        FieldOption.NOTNULLABLE
      } else {
        implicitOption
      }
    }

  }
}
