import sbt._
import Keys._
import Process._
import xml.XML
import org.sbtidea.SbtIdeaPlugin

object MessagePackScalaBuild extends Build {
  val messagePackVersion = "0.6.9"

  override lazy val settings = super.settings ++
      Seq(
        organization := "org.msgpack",
        name := "msgpack-scala",
        version := messagePackVersion,
        scalaVersion := "2.11.0-M7",
        scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature"),
        // crossScalaVersions := Seq("2.9.0-1","2.9.1","2.9.1-1","2.9.2"),
        resolvers ++= Seq(Resolver.mavenLocal),
        parallelExecution in Test := false,
        fork in Test := true
      )

  lazy val dependencies = Seq(
    "org.msgpack" % "msgpack" % messagePackVersion,
    "com.googlecode.json-simple" % "json-simple" % "1.1.1" % "compile",
    "org.javassist" % "javassist" % "3.16.1-GA" % "compile",
    "org.slf4j" % "slf4j-api" % "1.7.5" % "provided"
  )

  lazy val dependenciesForTest = Seq(
    "junit" % "junit" % "4.11" % "test",
    "ch.qos.logback" % "logback-core" % "1.0.13" % "test",
    "ch.qos.logback" % "logback-classic" % "1.0.13" % "test"
  )

  lazy val dependsOnScalaVersion = (scalaVersion) { v => {
    val specs = v match{
      case "2.9.3"  => "org.specs2" %% "specs2" % "1.12.4.1" % "test"
      case "2.9.2"  => "org.specs2" %% "specs2" % "1.12.3" % "test"
      case "2.9.1-1" => "org.specs2" %% "specs2" % "1.12.3" % "test"
      case "2.9.1"  => "org.specs2" %% "specs2" % "1.12.3" % "test"
      case "2.9.0-1"  => "org.specs2" %% "specs2" % "1.8.2" % "test"
      case "2.9.0"  => "org.specs2" %% "specs2" % "1.7.1" % "test"
      case x if x.startsWith("2.10") => "org.specs2" %% "specs2" % "1.14" % "test"
      case "2.11.0-M7" => "org.specs2" %% "specs2" % "2.3.6" % "test"
      case _ => "org.specs2" %% "specs2" % "1.8.2" % "test"
    }
    Seq(
      /*"org.scala-lang" % "scalap" % v,*/
      "org.scala-lang" % "scala-reflect" % v,
      specs.
        exclude("org.scala-lang.modules", "scala-xml_2.11.0-M6").
        exclude("org.scala-lang.modules", "scala-parser-combinators_2.11.0-M6")
    )
  }}

  lazy val root = Project(id = "msgpack-scala",
                          base = file("."),
                          settings = Project.defaultSettings ++ SbtIdeaPlugin.settings ++ Seq(
                            libraryDependencies ++= dependencies,
                            libraryDependencies ++= dependenciesForTest,
                            libraryDependencies <++= dependsOnScalaVersion,
                            publishMavenStyle := true,
                            publishArtifact in Test := false,
                            publishTo <<= version { (v: String) =>
                              val nexus = "https://oss.sonatype.org/"
                              if (v.trim.endsWith("SNAPSHOT"))
                                Some("snapshots" at nexus + "content/repositories/snapshots")
                              else
                                Some("releases"  at nexus + "service/local/staging/deploy/maven2")
                            },
                            pomIncludeRepository := { _ => false },
                            pomExtra := loadPomExtra()
                            )
  )

  def loadPomExtra() = {
    XML.loadFile( file( "./project/pomExtra.xml")).child
  }
}
