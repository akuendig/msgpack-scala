package org.msgpack.scalautil

import java.lang.reflect.{Type => JType, ParameterizedType => JParameterizedType}
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.runtime.universe._
import scala.language.reflectiveCalls

/**
 *
 * User: takeshita
 * Create: 12/04/03 1:22
 */

object ScalaSigUtil {

  case class Property(name: String, getter: MethodSymbol, setter: MethodSymbol, parent: Type) {

    // Find the type parameters for the parent, i.e. the class holding the property
    private val parentTypeParams = parent.typeSymbol.asClass.typeParams
    private val parentTypeArgs = parent match {
      case TypeRef(_, _, args) => args
    }
    private val parentTypeMap = (parentTypeParams, parentTypeArgs).zipped

    val tpe: Type =
      parentTypeMap.
        find(_._1.asType.toType =:= getter.returnType).
        map(_._2).
        getOrElse(getter.returnType)

//    println(s"$parent: ${getter.returnType} in ${parentTypeMap.mkString("[", ", ", "]")} is $tpe")

    def annotations(): List[Annotation] = {
      // If the getter is a proper Scala getter function, we
      // can get a reference to its backing field, the `accessed`.
      // Then we can also read the annotations of the field which is
      // where scala stores annotations for proper properties :)
      val scalaFieldAnnotations =
        if (getter.isGetter)
          setter.accessed.asTerm.annotations
        else
          Nil

      getter.annotations ++ scalaFieldAnnotations ++ setter.annotations
    }
  }

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

  private def getDefinedProperties(clazz: Class[_]): Seq[Property] = {
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

    // Possible setters are all methods, that take exactly one argument
    val oneArg = rest.filter(_.paramss.headOption.exists(_.size == 1))

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
    } yield Property(getter.name.encoded, getter, setter, tpe)

    properties.toSeq
  }

  private val cmx = cm.asInstanceOf[ {
    def methodToJava(sym: scala.reflect.internal.Symbols#MethodSymbol): java.lang.reflect.Method
  }]

  def toJavaMethod(m: MethodSymbol): java.lang.reflect.Method = cmx.methodToJava(
    m.asInstanceOf[scala.reflect.internal.Symbols#MethodSymbol]
  )

  def erasedJavaClass[T: TypeTag]: java.lang.Class[_] =
    toErasedJavaClass(typeOf[T])

  def javaClass[T: TypeTag]: java.lang.reflect.Type =
    toJavaClass(typeOf[T])

  def toErasedJavaClass(t: Type): java.lang.Class[_] =
    cm.runtimeClass(t)

  def toJavaClass(t: Type): java.lang.reflect.Type = t match {
    case TypeRef(prefix, clazz, genericParams) =>
      // If we have a scala primitive type, we need to lookup the corresponding
      // Java primitive type
      val primitive = t.typeSymbol.asClass.isPrimitive
      val nameMapper = if (primitive) mapToPrimitiveJavaName else mapToRefJavaName

      if (clazz.fullName == "scala.Array") {
        // We convert arrays into the special Java array notation.
        val name = toJavaClass(genericParams(0)) match {
          case c: Class[_] if c.isPrimitive => "[" + c.getName.toUpperCase.charAt(0)
          case c: Class[_] => "[L" + c.getName + ";"
          case c: JParameterizedType => "[L" + c.getRawType + ";"
        }

        // And lookup the corresponding Java class
        forName(name)
      } else if (clazz.fullName == "scala.Enumeration.Value") {
        // On Enums, we actually lookup the prefix type, the enum value is
        // contained in. (com.example.MyEnum.Value => com.example.MyEnum)
        // This is neccessary, because an Enumeration could extend eny type
        // and we would also need to serialize the base class of the Enumeration

        prefix match {
          case SingleType(_, name) => nameMapper(name.fullName)
        }
      } else if (genericParams.size == 0) {
        // If we have a non-generic class, we simply translate it
        nameMapper(clazz.fullName)
      } else {
        // If we have a generic type, we return a helper class, that returns
        // the correct "name", i.e. Java class name, of the parameterized class.
        new MyParameterizedType(
          nameMapper(clazz.fullName),
          genericParams.map(toJavaClass).toArray
        )
      }
    case _ =>
      // If we do not have a TypeRef, we simply try getting the Java class
      // from reflection.
      cm.runtimeClass(t)
  }

  def annotationArg(an: Annotation, name: String): Option[Constant] =
    an.javaArgs.get(TermName(name)).flatMap {
      case LiteralArgument(ct: Constant) => Some(ct)
      case _ => None
    }

  private def forName(name: String) = Class.forName(name)

  private trait MapToJavaName extends ((String) => Class[_]) {
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

  private val commonNameMaps: Seq[(String, Class[_])] = Seq(
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
    "scala.Unit" -> classOf[Unit],
    "scala.Nothing" -> classOf[Unit]
  )

  private object mapToRefJavaName extends MapToJavaName {
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

  private object mapToPrimitiveJavaName extends MapToJavaName {
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

  def getCompanionObjectClass(tpe: Type): Option[Class[_]] = {
    // Get the Java class because there way we correctly translate Enumeration Values.
    // We use a mirror to get back a java.lang.reflection.Class
    val javaClass =
      toJavaClass(tpe).asInstanceOf[Class[_]]

    // Get the companion symbol. The companion symbol is a TermSymbol, NOT a ClassSymbol.
    // Thus we use the typeSignature to get the class of the companion.
    val companion =
      cm.classSymbol(javaClass).companionSymbol

    // NoSymbol is returned if there is not companion symbol.
    if (companion == NoSymbol) None
    else Some(cm.runtimeClass(companion.typeSignature))
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


private class MyParameterizedType(rowClass: Class[_], paramClasses: Array[JType]) extends JParameterizedType {
  def getActualTypeArguments: Array[JType] = paramClasses

  def getRawType: JType = rowClass

  def getOwnerType: JType = null

  override def toString: String = {
    rowClass.toString + "<" + paramClasses.mkString(",") + ">"
  }
}

private object MyParameterizedType {
  def apply[T: TypeTag](): MyParameterizedType =
    apply(typeOf[T])

  def apply(t: Type): MyParameterizedType = {
    val clazz = cm.runtimeClass(t)
    val typeParams = t match {
      case TypeRef(_, _, args) => args
    }
    val jTypeParams = typeParams.map(tp => ScalaSigUtil.toJavaClass(tp)).toArray

    new MyParameterizedType(clazz, jTypeParams)
  }
}
