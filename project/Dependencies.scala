import sbt._

object Dependencies {
  val scalaEnvVersion = Option(System.getenv("SCALA_VERSION"))
  val scala212 = scalaEnvVersion.filter(isScala212).getOrElse("2.12.17")
  val scala213 = scalaEnvVersion.filter(isScala213).getOrElse("2.13.10")
  val scala30 = scalaEnvVersion.filter(isScala30).getOrElse("3.0.2")
  val scala31Plus = scalaEnvVersion.filter(isScala31Plus).getOrElse("3.2.2")
  val asmVersion = "9.5"
  val coursierVersion = "2.1.3"

  def isScala212(version: String): Boolean = version.startsWith("2.12")
  def isScala213(version: String): Boolean = version.startsWith("2.13")
  def isScala30(version: String): Boolean = version.startsWith("3.0")
  def isScala31Plus(version: String): Boolean = version.startsWith("3") && !isScala30(version)

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion

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
  val scalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.10.0"
  val sbtTestAgent = "org.scala-sbt" % "test-agent" % "1.8.2"

  // test dependencies
  val munit = "org.scalameta" %% "munit" % "1.0.0-M7"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
}
