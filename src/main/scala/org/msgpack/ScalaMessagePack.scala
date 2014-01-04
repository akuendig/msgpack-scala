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
package org.msgpack

import `type`.Value
import conversion.ValueConversions
import scalautil.ScalaSigUtil
import template._
import java.io.InputStream

/*
 * The core object of scala message pack
 * You should import
 * {{{
 * import org.msgpack.ScalaMessagePack._
 * }}}
 * then you can pack / unpack easily
 * {{{
 * val packedData = write(object) // or pack(object)
 * val unpackedObject = read[TargetClass](packedData) // or unpack[TargetClass](packedData)
 * }}}
 *
 * User: takeshita
 * Date: 11/03/10
 * Time: 1:34
 */
object ScalaMessagePack extends ScalaMessagePackWrapper with ValueConversions {

  /**
   * dummy init method
   */
  def init() = {}

  val messagePack = new ScalaMessagePack()
}


/**
 * Message pack implementation for scala
 */
class ScalaMessagePack extends MessagePack(new ScalaTemplateRegistry()) {}

/**
 * Supply utility methods for MessagePack
 * Method names are changed, because write and read method names often conflict when using filed importing
 *
 * name is changed because
 */
trait ScalaMessagePackWrapper {
  import scala.reflect.runtime.{ currentMirror => cm }
  import scala.reflect.runtime.universe.{ typeOf, TypeTag }

  def messagePack: MessagePack

  def write[T: TypeTag](obj: T): Array[Byte] = {
    val tpe = typeOf[T]

    if (tpe.typeSymbol.asClass.typeParams.size > 0) {
      val t = messagePack.lookup(ScalaSigUtil.javaClass[T])
      messagePack.write(obj, t.asInstanceOf[Template[T]])
    } else {
      messagePack.write(obj)
    }
  }

  def writeT[T](obj: T)(implicit template: Template[T]): Array[Byte] = {
    messagePack.write(obj, template)
  }

  def writeV(value: Value): Array[Byte] = {
    messagePack.write(value)
  }

  def read[T: TypeTag](data: Array[Byte]): T = {
    val tpe = typeOf[T]

    if (tpe.typeSymbol.asClass.typeParams.size > 0) {
      val t = messagePack.lookup(ScalaSigUtil.javaClass[T])
      messagePack.read(data, t).asInstanceOf[T]
    } else {
      messagePack.read(data, cm.runtimeClass(tpe)).asInstanceOf[T]
    }
  }

  def read[T: TypeTag](data: InputStream): T = {
    val tpe = typeOf[T]

    if (tpe.typeSymbol.asClass.typeParams.size > 0) {
      val t = messagePack.lookup(ScalaSigUtil.javaClass[T])
      messagePack.read(data, t).asInstanceOf[T]
    } else {
      messagePack.read(data, cm.runtimeClass(tpe)).asInstanceOf[T]
    }
  }

  def readTo[T](data: Array[Byte], obj: T): T = {
    messagePack.read(data, obj)
  }

  def readTo[T](data: InputStream, obj: T): T = {
    messagePack.read(data, obj)
  }

  def readAsValue(data: Array[Byte]): Value = {
    messagePack.read(data)
  }

  def readAsValue(data: InputStream): Value = {
    messagePack.read(data)
  }

  def convert[T: TypeTag](value: Value) = {
    val tpe = typeOf[T]

    messagePack.convert(value, cm.runtimeClass(tpe))
  }
}
