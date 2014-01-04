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
import java.lang.annotation.{Annotation => JAnnotation}
import org.msgpack.scalautil.ScalaSigUtil
import org.msgpack.scalautil.ScalaSigUtil.Property
import scala.reflect.runtime.universe._
import scala.collection.immutable.Queue

/**
 * Combination with java reflection and scalap.ScalaSigParser
 * User: takeshita
 * Create: 11/10/13 11:53
 */

trait ScalaPropertyFinder {
  self: AbstractTemplateBuilder =>

  override def toFieldEntries(targetClass: Class[_], from: FieldOption): Array[FieldEntry] = {
    // Find all declared or inherited properties
    val props = ScalaSigUtil.getAllProperties(targetClass)
    // Sort them according to the @index annotations
    val indexed = indexing(props)

    // Convert them to field entries
    val converted = indexed.map(convertToScalaFieldEntry).toArray[FieldEntry]

    converted
  }

  def indexing(props: Seq[Property]): Array[Property] = {
    // Create an empty list for all properties.
    val indexed = new Array[Property](props.size)

    // Create a queue for all properties, that have no @index annotation
    var notIndexed: Queue[Property] = Queue.empty

    // Read to all properties and filter out the ones
    // without @index into `notIndexed`, write the others
    // into the declared index in `indexed`.
    for (s <- props) {
      getAnnotation[Index](s) match {
        case None =>
          notIndexed = notIndexed.enqueue(s)
        case Some(indexAnnotation) =>
          val index = ScalaSigUtil.
            annotationArg(indexAnnotation, "value").
            get.
            value.
            asInstanceOf[java.lang.Integer]

          if (index < 0 || index >= indexed.length) {
            throw new TemplateBuildException("invalid index: %s index must be 0 <= x < %s".format(index, indexed.length))
          } else if (indexed(index) != null) {
            throw new TemplateBuildException("duplicated index: " + index)
          } else {
            indexed(index) = s
          }
      }
    }

    // Fill empty spots in `indexed` with properties of `notIndexed`.
    // We try to retain ordering, as received from reflection.
    indexed.map {
      e =>
        if (e == null && !notIndexed.isEmpty) {
          val (head, rest) = notIndexed.dequeue
          notIndexed = rest
          head
        } else {
          e
        }
    }
  }

  def hasAnnotation[T <: JAnnotation : TypeTag](prop: Property): Boolean = {
    prop.
      annotations().
      exists(_.tpe =:= typeTag[T].tpe)
  }

  def getAnnotation[T <: JAnnotation : TypeTag](prop: Property): Option[Annotation] = {
    prop.
      annotations().
      find(_.tpe =:= typeTag[T].tpe)
  }

  def convertToScalaFieldEntry(propInfo: Property): ScalaFieldEntry = {
    val Property(name, getter, setter, _) = propInfo

    // Depending on the return type of the getter, i.e. the type
    // of the property, we return a different version of ScalaFieldEntry.
    propInfo.tpe match {
      // We special case Scala Enumerations, as they are constructed
      // specially. The generic type is actually the type of the companion object.
      case t if t.typeSymbol.fullName == "scala.Enumeration.Value" =>
        new ScalaFieldEntry(name,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          ScalaSigUtil.toErasedJavaClass(t),
          ScalaSigUtil.getCompanionObjectClass(t).get,
          ScalaSigUtil.toJavaMethod(getter),
          ScalaSigUtil.toJavaMethod(setter)
        )
      // A parameterized type contains type arguments, it is generic.
      // For parameterized types we use the erased type as the normal type
      // and the fully specified type as the generic type.
      case t =>
        new ScalaFieldEntry(name,
          readFieldOption(propInfo, FieldOption.DEFAULT),
          ScalaSigUtil.toErasedJavaClass(t),
          ScalaSigUtil.toJavaClass(t),
          ScalaSigUtil.toJavaMethod(getter),
          ScalaSigUtil.toJavaMethod(setter)
        )
    }
  }

  def readValueType(prop: Property): ClassSymbol =
    prop.tpe.typeSymbol.asClass

  def readFieldOption(prop: Property, defaultOption: FieldOption): FieldOption =
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
        defaultOption
      }
    }
}
