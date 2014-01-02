//
//  MessagePack for Scala
//
//  Copyright (C) 2009-2011 FURUHASHI Sadayuki
//
//     Licensed under the Apache License, Version 2.0 (the "License");
//     you may not use this file except in compliance with the License.
//     You may obtain a copy of the License at
//
//         http://www.apache.org/licenses/LICENSE-2.0
//
//     Unless required by applicable law or agreed to in writing, software
//     distributed under the License is distributed on an "AS IS" BASIS,
//     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//     See the License for the specific language governing permissions and
//     limitations under the License.
//

package org.msgpack.conversion

import org.msgpack.scalautil.MyParameterizedType
import org.msgpack.MessagePack
import org.msgpack.`type`.{ValueFactory, Value}
import scala.reflect.runtime.universe._

/**
 *
 * User: takeshita
 * Create: 11/10/14 13:07
 */

class RichValue(messagePack: MessagePack, value: Value) {

  def apply(index: Int): Value = {
    if (value.isMapValue) {
      apply(ValueFactory.createIntegerValue(index))
    } else {
      value.asArrayValue().get(index)
    }
  }

  def apply(value: Value): Value = {
    value.asMapValue().get(value)
  }

  def toValueArray: Array[Value] = {
    value.asArrayValue().getElementArray
  }

  def toValueList: List[Value] = {
    toValueArray.toList
  }

  def toValueMap: Map[Value, Value] = {
    value.
      asMapValue().
      getKeyValueArray.
      grouped(2).
      map(p => p(0) -> p(1)).
      toMap
  }

  def asArray[T: TypeTag]: Array[T] = {
    val templ = messagePack.lookup(MyParameterizedType[Array[T]]())

    messagePack.convert(value, templ).asInstanceOf[Array[T]]
  }

  def asList[T: TypeTag]: List[T] = {
    val templ = messagePack.lookup(MyParameterizedType[List[T]]())

    messagePack.convert(value, templ).asInstanceOf[List[T]]
  }

  def as[T: TypeTag]: T = {
    val templ = messagePack.lookup(MyParameterizedType[T]())

    messagePack.convert(value, templ).asInstanceOf[T]
  }

  def asMap[K: TypeTag, V: TypeTag]: Map[K, V] = {
    val keyTemp = messagePack.lookup(MyParameterizedType[K]())
    val valTemp = messagePack.lookup(MyParameterizedType[V]())

    val entries = for {
      Array(key, value) <- value.asMapValue().getKeyValueArray.grouped(2)
    } yield (
        messagePack.convert(key, keyTemp).asInstanceOf[K],
        messagePack.convert(value, valTemp).asInstanceOf[V]
        )

    entries.toMap
  }

  override def toString: String = {
    value.toString
  }


  def asByte(): Byte = {
    value.asIntegerValue().getByte
  }

  def asShort(): Short = {
    value.asIntegerValue().getShort
  }

  def asInt(): Int = {
    value.asIntegerValue().getInt
  }

  def asLong(): Long = {
    value.asIntegerValue().getLong
  }

  def asDouble(): Double = {
    value.asFloatValue.getDouble
  }

  def asFloat(): Float = {
    value.asFloatValue.getFloat
  }

  def asBool(): Boolean = {
    value.asBooleanValue.getBoolean
  }

  def asString(): String = {
    value.asRawValue().getString
  }


}
