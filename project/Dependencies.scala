import sbt._

object Dependencies {
  //def scala210 = "2.10.7"
  //def scala211 = "2.11.12"
  def scala212 = "2.12.12"
  //def scala213 = "2.13.4"
  //val currentScalaVersion = scala213

  val asmVersion = "7.0"
  val bspVersion = "2.0.0-M13"
  val javaDebugVersion = "0.21.0+1-7f1080f1"
  val monixVersion = "2.3.3"
  val sbtTestInterfaceVersion = "1.0"
  val scalametaVersion = "4.4.2"
  val zincVersion = "1.3.0-M4+46-edbe573e"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val bsp4j = "ch.epfl.scala" % "bsp4j" % bspVersion
  val bsp4s = "ch.epfl.scala" %% "bsp4s" % bspVersion // only 2.12 published

  val javaDebug = "ch.epfl.scala" % "com-microsoft-java-debug-core" % javaDebugVersion
  val monix = "io.monix" %% "monix" % monixVersion
  val sbtTestInterface = "org.scala-sbt" % "test-interface" % sbtTestInterfaceVersion
  val scalameta = "org.scalameta" %% "scalameta" % scalametaVersion
  val zinc = "ch.epfl.scala" %% "zinc" % zincVersion
}
