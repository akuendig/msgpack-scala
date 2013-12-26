package org.msgpack.scalautil

import java.lang.reflect.{Field, Method, Type => JType, ParameterizedType}
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._

/**
 *
 * User: takeshita
 * Create: 12/04/03 1:22
 */

object ScalaSigUtil {

  type Property = (Method, Method, Field, MethodSymbol)
  type PropertySet = (String, Property)

  def getAllProperties(clazz: Class[_]): Seq[PropertySet] = {

    def superClassProps = {
      val superClass = clazz.getSuperclass
      if (superClass == null || superClass == classOf[java.lang.Object]) {
        Nil
      } else {
        getAllProperties(superClass)
      }
    }

    def interfaceProps = {
      clazz.getInterfaces.foldLeft(Seq[PropertySet]())((l, i) => l ++ getAllProperties(i))
    }

    superClassProps ++ interfaceProps ++ getDefinedProperties(clazz)
  }

  def getDefinedProperties(clazz: Class[_]): Seq[PropertySet] = {
    val tpe = cm.classSymbol(clazz).toType
    val fieldMap = clazz.getFields.map(f => f.getName -> f).toMap

    val properties = tpe.declarations.collect {
      case m if m.isMethod && m.asMethod.isGetter => {
        val field = m.asMethod.accessed.asTerm
        val getter = field.getter.asMethod
        val setterOpt = Option(field.setter).map(_.asMethod)

        val javaField = fieldMap(field.name.decoded)
        val javaGetter = clazz.getMethod(getter.name.encoded)
        val javaSetterOpt = setterOpt.map(s => clazz.getMethod(s.name.encoded))

        val p: Property = (
          javaGetter,
          javaSetterOpt.getOrElse(null),
          javaField,
          getter
          )

        (getter.name.encoded, p)
      }
    }

    properties.toSeq
  }

  def getReturnType(methodSymbol: MethodSymbol): Option[JType] = {
    methodSymbol.returnType match {
      case NullaryMethodType(returnType) => toJavaClass(returnType)
      case MethodType(methodParams, returnType) => toJavaClass(returnType)
      case trt: TypeRef => toJavaClass(trt)
      case _ => {
        None
      }
    }
  }

  def toJavaClass(t: Type, primitive_? : Boolean = true): Option[JType] = t match {
    case TypeRef(pre, sym, genericParams) => {
      val nameMapper = if (primitive_?) mapToPrimitiveJavaName else mapToRefJavaName
      if (sym.fullName == "scala.Array") {
        toJavaClass(genericParams(0), true) match {
          case Some(c: Class[_]) => if (c.isPrimitive) Some(forName("[" + c.getName.toUpperCase.charAt(0))) else Some(forName("[L" + c.getName + ";"))
          case Some(c: ParameterizedType) => Some(forName("[L" + c.getRawType + ";"))
          case _ => throw new Exception("Never match here")
        }
      } else if (sym.fullName == "scala.Enumeration.Value") {
        pre match {
          case SingleType(_, name) => {
            Some(nameMapper(name.fullName))
          }
        }
      } else if (genericParams.size == 0) {
        Some(nameMapper(sym.fullName))
      } else {
        Some(new MyParameterizedType(
          nameMapper(sym.fullName),
          genericParams.map(p => toJavaClass(p, false).get).toArray))
      }
    }
    case _ => {
      None
    }

  }

  def forName(name: String) = Class.forName(name)

  trait MapToJavaName extends ((String) => Class[_]) {
    val nameMap: Map[String, Class[_]]

    def apply(scalaClassName: String): Class[_] = {
      if (scalaClassName.startsWith("scala")) {
        nameMap.getOrElse(scalaClassName, forName(scalaClassName))
      } else {
        if (scalaClassName.startsWith("<empty>.")) forName(scalaClassName.substring(8))
        else forName(scalaClassName)
      }
    }
  }


  val commonNameMaps: Seq[(String, Class[_])] = Seq(
    "scala.Predef.String" -> classOf[java.lang.String],
    "scala.Predef.Map" -> classOf[Map[_, _]],
    "scala.Predef.Seq" -> classOf[Seq[_]],
    "scala.Predef.Set" -> classOf[Set[_]],
    "scala.package.List" -> classOf[List[_]],
    "scala.Unit" ->classOf[java.lang.Void],
    "scala.package.Seq" -> classOf[Seq[_]],
    "scala.package.Either" -> classOf[Either[_,_]]
  )

  object mapToRefJavaName extends MapToJavaName {
    val nameMap = commonNameMaps ++ Seq(
      "scala.Int" -> classOf[java.lang.Integer],
      "scala.Byte" -> classOf[java.lang.Byte],
      "scala.Short" -> classOf[java.lang.Short],
      "scala.Long" -> classOf[java.lang.Long],
      "scala.Float" -> classOf[java.lang.Float],
      "scala.Double" -> classOf[java.lang.Double],
      "scala.Boolean" -> classOf[java.lang.Boolean]
    ) toMap
  }

  object mapToPrimitiveJavaName extends MapToJavaName {
    val nameMap = commonNameMaps ++ Seq(
      "scala.Int" -> java.lang.Integer.TYPE,
      "scala.Byte" -> java.lang.Byte.TYPE,
      "scala.Short" -> java.lang.Short.TYPE,
      "scala.Long" -> java.lang.Long.TYPE,
      "scala.Float" -> java.lang.Float.TYPE,
      "scala.Double" -> java.lang.Double.TYPE,
      "scala.Boolean" -> java.lang.Boolean.TYPE
    ) toMap
  }

  def getCompanionObjectClass(clazz: Class[_]): Option[Class[_]] = {
    if (clazz.getName.endsWith("$")) None
    else {
      try {
        val c = Class.forName(clazz.getName + "$")
        Some(c)
      } catch {
        case e: NoClassDefFoundError => {
          None
        }
        case e: ClassNotFoundException => {
          None
        }
      }
    }
  }

  def reverseCompanionObjectClass(clazz: Class[_]): Option[Class[_]] = {
    if (!clazz.getName.endsWith("$")) None
    else {
      try {
        val c = Class.forName(clazz.getName.substring(0, clazz.getName.length - 1))
        Some(c)
      } catch {
        case e: NoClassDefFoundError => {
          None
        }
        case e: ClassNotFoundException => {
          None
        }
      }
    }
  }

}


class MyParameterizedType(rowClass: Class[_], paramClasses: Array[JType]) extends ParameterizedType {
  def getActualTypeArguments: Array[JType] = paramClasses

  def getRawType: JType = rowClass

  def getOwnerType: JType = null

  override def toString: String = {
    rowClass.toString + "<" + paramClasses.mkString(",") + ">"
  }
}

object MyParameterizedType {
  def apply(m: Manifest[_]): MyParameterizedType = {
    new MyParameterizedType(m.erasure, m.typeArguments.map(MyParameterizedType.apply(_)).toArray)

  }
}
