package org.msgpack.scalautil

import java.lang.reflect.{Field, Method, Type => JType, ParameterizedType}
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import org.slf4j.{LoggerFactory, Logger}

/**
 *
 * User: takeshita
 * Create: 12/04/03 1:22
 */

object ScalaSigUtil {
  private val logger : Logger = LoggerFactory.getLogger(getClass)

  type Property = (Method, Method, Field, MethodSymbol)
  type PropertySet = (String, Property)

  val SetterSuffix = "_="

  def getAllProperties(clazz: Class[_]): Seq[PropertySet] = {

    def superClassProps = {
      val superClass: Class[_] = clazz.getSuperclass
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
    logger.debug("Inspecting class {} for defined properties.", clazz.getName)

    val tpe = cm.classSymbol(clazz).toType
    val fieldMap = clazz.getFields.map(f => f.getName -> f).toMap

    // Find all zero argument methods, that are public.
    // Java does not provide info on private methods via reflection.
    val (zeroArgs, rest) = tpe.declarations.collect {
      case m if m.isMethod && m.asMethod.isPublic => m.asMethod
    }.partition(_.paramss.headOption.map(_.size == 0).getOrElse(true))

    // Build a map to speed up lookup of potential getters
    val zeroArgsMap = zeroArgs.map(m => m.name.decoded -> m).toMap

    logger.debug("The class contains the following zero argument methods: {}", zeroArgs.map(_.name.decoded))

    // Possible setters are all methods, that take exactly one argument
    val oneArg = rest.filter(_.paramss.headOption.exists(_.size == 1))

    // Create the list of all setters, which have a getter with the same
    // name and one parameter with the type of the return type of the getter.
    val settersWithGetters = for {
      setter <- oneArg

      // Take the decoded name, i.e. prop_=
      name = setter.name.decoded

      // Test if the potential setter end with _=
      if name.endsWith(SetterSuffix)

      // Store the name the getter would have
      cleanName = name.substring(0, name.length - SetterSuffix.length)

      // See if we have a zero argument method with the same name
      if zeroArgsMap.contains(cleanName)
      getter = zeroArgsMap(cleanName)

      // Test if that method has the correct return type
      if getter.returnType =:= setter.paramss.head.head.typeSignature
    } yield (cleanName, getter, setter)

    logger.debug("The class contains the following one argument methods: {}", oneArg.map(_.name.decoded))

    // Convert the Scala reflection information into Java reflection information
    val properties = settersWithGetters.map {
      case (cleanName, getter, setter) =>
        val javaField = fieldMap.getOrElse(cleanName, null)
        val javaGetter = clazz.getMethod(getter.name.encoded)
        val javaSetter = {
          val param1 = setter.paramss(0)(0).asTerm
          val param1Tpe = cm.runtimeClass(param1.typeSignature)
          clazz.getMethod(setter.name.encoded, param1Tpe)
        }

        val p: Property = (
          javaGetter,
          javaSetter,
          javaField,
          getter
        )

        (getter.name.encoded, p)
    }

    logger.debug("The extracted properties are: {}", properties.map(_._1))

    properties.toSeq
  }

  def getReturnType(methodSymbol: MethodSymbol): Option[JType] = {
    methodSymbol.returnType match {
      case NullaryMethodType(returnType) => toJavaClass(returnType)
      case MethodType(methodParams, returnType) => toJavaClass(returnType)
      case trt: TypeRef => toJavaClass(trt)
      case _ => None
    }
  }

  def toJavaClass(t: Type, primitive_? : Boolean = true): Option[JType] = t match {
    case TypeRef(pre, sym, genericParams) =>
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
    case _ =>
      None
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
    "scala.Predef.List" -> classOf[List[_]],
    "scala.Predef.Map" -> classOf[Map[_, _]],
    "scala.Predef.Seq" -> classOf[Seq[_]],
    "scala.Predef.Set" -> classOf[Set[_]],
    "scala.Predef.Either" -> classOf[Either[_, _]],
    "scala.List" -> classOf[List[_]],
    "scala.Map" -> classOf[Map[_, _]],
    "scala.Seq" -> classOf[Seq[_]],
    "scala.Set" -> classOf[Set[_]],
    "scala.Either" -> classOf[Either[_, _]],
    "scala.package.List" -> classOf[List[_]],
    "scala.package.Map" -> classOf[Map[_, _]],
    "scala.package.Seq" -> classOf[Seq[_]],
    "scala.package.Set" -> classOf[Set[_]],
    "scala.package.Either" -> classOf[Either[_, _]],
    "scala.Unit" -> classOf[java.lang.Void]
  )

  object mapToRefJavaName extends MapToJavaName {
    val nameMap = (commonNameMaps ++ Seq(
      "scala.Int" -> classOf[java.lang.Integer],
      "scala.Byte" -> classOf[java.lang.Byte],
      "scala.Short" -> classOf[java.lang.Short],
      "scala.Long" -> classOf[java.lang.Long],
      "scala.Float" -> classOf[java.lang.Float],
      "scala.Double" -> classOf[java.lang.Double],
      "scala.Boolean" -> classOf[java.lang.Boolean]
    )).toMap
  }

  object mapToPrimitiveJavaName extends MapToJavaName {
    val nameMap = (commonNameMaps ++ Seq(
      "scala.Int" -> java.lang.Integer.TYPE,
      "scala.Byte" -> java.lang.Byte.TYPE,
      "scala.Short" -> java.lang.Short.TYPE,
      "scala.Long" -> java.lang.Long.TYPE,
      "scala.Float" -> java.lang.Float.TYPE,
      "scala.Double" -> java.lang.Double.TYPE,
      "scala.Boolean" -> java.lang.Boolean.TYPE
    )).toMap
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
        case e: NoClassDefFoundError => None
        case e: ClassNotFoundException => None
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
    new MyParameterizedType(m.runtimeClass, m.typeArguments.map(MyParameterizedType.apply).toArray)
  }
}
