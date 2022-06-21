package ch.epfl.scala.debugadapter

import coursier._

sealed trait ScalaVersion {
  def organization: Organization = Organization("org.scala-lang")
  def version: String
  def compiler: Dependency
  def library: Dependency
  def compilerMain: String
  def binaryVersion: String
}

final case class Scala2(version: String) extends ScalaVersion {
  def compiler: Dependency =
    Dependency(Module(organization, name"scala-compiler"), version)
  def library: Dependency =
    Dependency(Module(organization, name"scala-library"), version)
  def compilerMain: String = "scala.tools.nsc.Main"
  def binaryVersion: String =
    version.split('.').take(2).mkString(".")
}

final case class Scala3(version: String) extends ScalaVersion {
  def compiler: Dependency =
    Dependency(Module(organization, name"scala3-compiler_3"), version)
  def library: Dependency =
    Dependency(Module(organization, name"scala3-library_3"), version)
  def compilerMain: String = "dotty.tools.dotc.Main"
  def binaryVersion: String =
    if (version.endsWith("-bin-SNAPSHOT"))
      version.stripSuffix("-bin-SNAPSHOT")
    else "3"
}

object ScalaVersion {
  val `2.12` = Scala2("2.12.16")
  val `2.13` = Scala2("2.13.8")
  val `3.0` = Scala3("3.0.2")
  val `3.1` = Scala3("3.1.3")
}
