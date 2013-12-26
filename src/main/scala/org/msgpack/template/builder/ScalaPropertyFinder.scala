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
import collection.immutable.ListMap
import org.msgpack.annotation.{NotNullable, Optional, Index, Ignore}
import java.lang.annotation.{ Annotation => JAnnotation}
import java.lang.reflect.{Modifier, Field, Method, Type => JType, ParameterizedType}
import org.msgpack.scalautil.ScalaSigUtil
import org.msgpack.scalautil.ScalaSigUtil.PropertySet

/**
 * Combination with java reflection and scalap.ScalaSigParser
 * User: takeshita
 * Create: 11/10/13 11:53
 */

trait ScalaPropertyFinder{

  self : AbstractTemplateBuilder =>

  val SetterSuffix = "_$eq"

  override def toFieldEntries(targetClass: Class[_], from: FieldOption) : Array[FieldEntry] = {
    val props = ScalaSigUtil.getAllProperties(targetClass)

    val propertySetSeq = props.filter(ps => !hasAnnotation(ps, classOf[Ignore]))
    val indexed = indexing(propertySetSeq)

    indexed.map(convertToScalaFieldEntry(_))
  }

  def indexing(props: Seq[PropertySet]): Array[PropertySet] = {
    val indexed = new Array[PropertySet](props.size)

    var notIndexed: List[PropertySet] = Nil

    for (s <- props) {
      val i = getAnnotation(s, classOf[Index])
      if (i == null) {
        notIndexed = notIndexed :+ s
      } else {
        val index = i.value
        if (indexed(index) != null) {
          throw new TemplateBuildException("duplicated index: " + index);
        } else {
          try {
            indexed(index) = s
          } catch {
            case e: Exception => {
              throw new TemplateBuildException("invalid index: %s index must be 0 <= x < %s".format(index, indexed.length));
            }
          }
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

  def hasAnnotation[T <: JAnnotation](prop: PropertySet, classOfAnno: Class[T]): Boolean = {
    val getter = prop._2._1
    val setter = prop._2._2
    val field = prop._2._3
    getter.getAnnotation(classOfAnno) != null ||
      setter.getAnnotation(classOfAnno) != null || {
      if (field != null) field.getAnnotation(classOfAnno) != null
      else false
    }
  }

  def getAnnotation[T <: JAnnotation](prop: PropertySet, classOfAnno: Class[T]): T = {
    val getter = prop._2._1
    val setter = prop._2._2
    val field = prop._2._3



    val a = getter.getAnnotation(classOfAnno)
    if (a != null) {
      a
    } else {
      val b = setter.getAnnotation(classOfAnno)
      if (b != null) {
        b
      } else if (field != null) {
        field.getAnnotation(classOfAnno)
      } else {
        null.asInstanceOf[T]
      }
    }
  }

  def convertToScalaFieldEntry(propInfo: PropertySet) = {
    val getter = propInfo._2._1
    getter.getGenericReturnType match{
      case pt : ParameterizedType => {
        new ScalaFieldEntry(propInfo._1,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          getter.getReturnType,
          ScalaSigUtil.getReturnType(propInfo._2._4).get,
          propInfo._2._1,
          propInfo._2._2
        )
      }
      case t if t.asInstanceOf[Class[_]].getName == "scala.Enumeration$Value" => {
        new ScalaFieldEntry(propInfo._1,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          getter.getReturnType,
          ScalaSigUtil.getCompanionObjectClass(
            ScalaSigUtil.getReturnType(propInfo._2._4).get.asInstanceOf[Class[_]]).get,
          propInfo._2._1,
          propInfo._2._2
        )
      }
      case t => {
        new ScalaFieldEntry(propInfo._1,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          getter.getReturnType,
          t,
          propInfo._2._1,
          propInfo._2._2
        )
      }
    }
  }

  def readValueType(prop: PropertySet) = {
    prop._2._1.getReturnType
  }

  def readFieldOption(prop: PropertySet, implicitOption: FieldOption) = {
    if (hasAnnotation(prop, classOf[Optional])) {
      FieldOption.OPTIONAL
    } else if (hasAnnotation(prop, classOf[NotNullable])) {
      FieldOption.NOTNULLABLE
    } else if (hasAnnotation(prop, classOf[Ignore])) {
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
