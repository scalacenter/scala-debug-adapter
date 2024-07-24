import sbt._

object Dependencies {
  val scalaEnvVersion = Option(System.getenv("SCALA_VERSION"))
  val scala212 = scalaEnvVersion.filter(isScala212).getOrElse("2.12.19")
  val scala213 = scalaEnvVersion.filter(isScala213).getOrElse("2.13.14")
  val scala30 = scalaEnvVersion.filter(isScala30).getOrElse("3.0.2")
  val scala31Plus = scalaEnvVersion.filter(isScala33).getOrElse("3.3.3")
  val scala34Plus = scalaEnvVersion.filter(isScala34).getOrElse("3.5.1-RC1")
  val asmVersion = "9.7"
  val coursierVersion = "2.1.10"

  def isScala212(version: String): Boolean = version.startsWith("2.12")
  def isScala213(version: String): Boolean = version.startsWith("2.13")
  def isScala30(version: String): Boolean = version.startsWith("3.0")
  def isScala33(version: String): Boolean =
    version.startsWith("3.1") || version.startsWith("3.2") || version.startsWith("3.3")
  def isScala34(version: String): Boolean = version.startsWith("3") && !isScala30(version) && !isScala33(version)

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
  val scalaCollectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.12.0"
  val sbtTestAgent = "org.scala-sbt" % "test-agent" % "1.10.0"
  val scalaMeta = ("org.scalameta" %% "parsers" % "4.9.5").cross(CrossVersion.for3Use2_13)

  // test dependencies
  val munit = "org.scalameta" %% "munit" % "1.0.0-RC1"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
}
