import sbt._

object Dependencies {
  def scala212 = "2.12.12"

  val asmVersion = "7.0"
  val javaDebugVersion = "0.21.0+1-7f1080f1"
  val sbtTestInterfaceVersion = "1.0"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion

  val javaDebug = "ch.epfl.scala" % "com-microsoft-java-debug-core" % javaDebugVersion
}
