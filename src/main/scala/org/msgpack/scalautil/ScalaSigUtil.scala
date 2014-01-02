package org.msgpack.scalautil

import java.lang.reflect.{Type => JType, ParameterizedType}
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import org.slf4j.{LoggerFactory, Logger}

/**
 *
 * User: takeshita
 * Create: 12/04/03 1:22
 */

object ScalaSigUtil {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  case class Property(name: String, getter: MethodSymbol, setter: MethodSymbol, field: Option[TermSymbol])

  val SetterSuffix = "_="

  def getAllProperties(clazz: Class[_]): Seq[Property] = {

    def superClassProps = {
      val superClass: Class[_] = clazz.getSuperclass
      if (superClass == null || superClass == classOf[java.lang.Object]) {
        Nil
      } else {
        getAllProperties(superClass)
      }
    }

    def interfaceProps = {
      clazz.getInterfaces.foldLeft(Seq[Property]())((l, i) => l ++ getAllProperties(i))
    }

    superClassProps ++ interfaceProps ++ getDefinedProperties(clazz)
  }

  def getDefinedProperties(clazz: Class[_]): Seq[Property] = {
    //    logger.debug("Inspecting class {} for defined properties.", clazz.getName)

    val tpe = cm.classSymbol(clazz).toType

    val fieldMap = tpe.declarations.collect {
      case f if f.isTerm && f.asTerm.isPublic =>
        f.name.decoded -> f.asTerm
    }.toMap

    // Find all zero argument methods, that are public.
    // Java does not provide info on private methods via reflection.
    val (zeroArgs, rest) = tpe.declarations.collect {
      case m if m.isMethod && m.asMethod.isPublic => m.asMethod
    }.partition(_.paramss.headOption.map(_.size == 0).getOrElse(true))

    // Build a map to speed up lookup of potential getters
    val zeroArgsMap = zeroArgs.map(m => m.name.decoded -> m).toMap

    //    logger.debug("The class contains the following zero argument methods: {}", zeroArgs.map(_.name.decoded))

    // Possible setters are all methods, that take exactly one argument
    val oneArg = rest.filter(_.paramss.headOption.exists(_.size == 1))

    //    logger.debug("The class contains the following one argument methods: {}", oneArg.map(_.name.decoded))

    // Create the list of all setters, which have a getter with the same
    // name and one parameter with the type of the return type of the getter.
    val properties = for {
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
    } yield Property(cleanName, getter, setter, fieldMap.get(cleanName))

    //    logger.debug("The extracted properties are: {}", properties.map(_.name))

    properties.toSeq
  }

  private val cmx = cm.asInstanceOf[ {
    def methodToJava(sym: scala.reflect.internal.Symbols#MethodSymbol): java.lang.reflect.Method
  }]

  def toJavaMethod(m: MethodSymbol): java.lang.reflect.Method = cmx.methodToJava(
    m.asInstanceOf[scala.reflect.internal.Symbols#MethodSymbol]
  )

  def toErasedJavaClass(t: Type): java.lang.Class[_] =
    cm.runtimeClass(t)

  def toJavaClass(t: Type): java.lang.reflect.Type = t match {
    case TypeRef(prefix, clazz, genericParams) => {
      val primitive = t.typeSymbol.asClass.isPrimitive
      val nameMapper = if (primitive) mapToPrimitiveJavaName else mapToRefJavaName

      if (clazz.fullName == "scala.Array") {
        val name = toJavaClass(genericParams(0)) match {
          case c: Class[_] if c.isPrimitive => "[" + c.getName.toUpperCase.charAt(0)
          case c: Class[_] => "[L" + c.getName + ";"
          case c: ParameterizedType => "[L" + c.getRawType + ";"
        }

        forName(name)
      } else if (clazz.fullName == "scala.Enumeration.Value") {
        prefix match {
          case SingleType(_, name) => {
            nameMapper(name.fullName)
          }
        }
      } else if (genericParams.size == 0) {
        nameMapper(clazz.fullName)
      } else {
        new MyParameterizedType(
          nameMapper(clazz.fullName),
          genericParams.map(toJavaClass).toArray
        )
      }
    }
    case _ => {
      cm.runtimeClass(t)
    }
  }

  def annotationArg(an: Annotation, name: String): Option[Constant] =
    an.javaArgs.get(TermName(name)).flatMap {
      case LiteralArgument(ct: Constant) => Some(ct)
      case _ => None
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

  def getCompanionObjectClass(tpe: Type): Option[Type] = {
    // Get the Java class because that way we correctly transalte enum
    // Values. We use a mirror to get back
    val javaClass =
      toJavaClass(tpe).asInstanceOf[Class[_]]
    val companion =
      cm.classSymbol(javaClass).companionSymbol

    if (companion == NoSymbol) {
      None
    } else
      Some(companion.typeSignature)
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
