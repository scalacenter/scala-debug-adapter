import sbt._

object Dependencies {
  val scala212 = "2.12.16"
  val scala213 = "2.13.10"
  val scala30 = "3.0.2"
  val scala32 = "3.2.0"
  val asmVersion = "9.4"
  val coursierVersion = "2.1.0-M7"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val javaDebug = "ch.epfl.scala" % "com-microsoft-java-debug-core" % "0.34.0+8"

  def scalaCompiler(scalaVersion: String): ModuleID = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) => "org.scala-lang" %% "scala3-compiler" % scalaVersion
      case _ => "org.scala-lang" % "scala-compiler" % scalaVersion
    }
  }

  def scalaReflect(scalaVersion: String): ModuleID = {
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) => "org.scala-lang" % "scala-reflect" % scala213
      case _ => "org.scala-lang" % "scala-reflect" % scalaVersion
    }
  }

  val scalaParallelCollection = "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  val scalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.8.1"
  val sbtTestAgent = "org.scala-sbt" % "test-agent" % "1.7.1"

  // test dependencies
  val munit = "org.scalameta" %% "munit" % "1.0.0-M6"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
}
