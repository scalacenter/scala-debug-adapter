import sbt._

object Dependencies {
  val scala212 = "2.12.12"
  val asmVersion = "7.3.1"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val javaDebug = "ch.epfl.scala" % "com-microsoft-java-debug-core" % "0.32.0+1"
  val utest = "com.lihaoyi" %% "utest" % "0.6.9"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scala212
  val sbtIo = "org.scala-sbt" %% "io" % "1.5.1"
  val sbtTestInterface = "org.scala-sbt" % "test-interface" % "1.0"
  val coursier = "io.get-coursier" %% "coursier" % "2.0.16"
}
