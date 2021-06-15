package ch.epfl.scala.debugadapter

import coursier._

sealed trait ScalaVersion {
  def version: String
  def compiler: Dependency
  def library: Dependency
  def compilerMain: String
}

final case class Scala2(version: String) extends ScalaVersion {
  val compiler: Dependency = Dependency(mod"org.scala-lang:scala-compiler", version)
  val library: Dependency = Dependency(mod"org.scala-lang:scala-library", version)
  val compilerMain: String = "scala.tools.nsc.Main"
}

final case class Scala3(version: String) extends ScalaVersion {
  val compiler: Dependency = Dependency(mod"org.scala-lang:scala3-compiler_3", version)
  val library: Dependency = Dependency(mod"org.scala-lang:scala3-library_3", version)
  val compilerMain: String = "dotty.tools.dotc.Main"
}

object ScalaVersion {
  val `2.12` = Scala2("2.13.6")
  val `2.13` = Scala2("2.13.6")
  val `3` = Scala3("3.0.0")
}
