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

import java.lang.Class
import org.msgpack.template._

/**
 *
 * User: takeshita
 * Create: 11/10/12 16:39
 */

class JavassistScalaTemplateBuilder(_registry: TemplateRegistry, classLoader: ClassLoader)
  extends JavassistTemplateBuilder(_registry, classLoader) with ScalaObjectMatcher with ScalaPropertyFinder {

  def this(r: TemplateRegistry) = this(r, null)

  val classPool = this.pool

  // build template
  override def buildTemplate[T](targetClass: Class[T], entries: Array[FieldEntry]) = {
    val templates = toTemplates(entries)
    val sEntries = entries.map(_.asInstanceOf[ScalaFieldEntry])

    createBuildContext().buildTemplate(targetClass, sEntries, templates).asInstanceOf[Template[T]]
  }

  private def toTemplates[T](entries: Array[FieldEntry]): Array[Template[_]] =
    entries.collect {
      case e if e.isAvailable =>
        registry.lookup(e.getGenericType)
    }

  // builder context
  override def createBuildContext() = {
    new ScalaBuildContext(this)
  }
}

abstract class JavassistScalaTemplate[T](var targetClass: Class[T], var templates: Array[Template[_]]) extends AbstractTemplate[T] {

  override def toString = {

    "JavassistScalaTemplate for:" + targetClass + "\n" +
      "Fields:\n" +
      templates.map(t => {
        t.toString
      }).mkString("\n")
  }

}
