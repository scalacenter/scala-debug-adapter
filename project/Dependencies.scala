import sbt._

object Dependencies {
  def scala212 = "2.12.12"
  val asmVersion = "7.0"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val javaDebug = "ch.epfl.scala" % "com-microsoft-java-debug-core" % "0.21.0+1-7f1080f1"
  val utest = "com.lihaoyi" %% "utest" % "0.6.6"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scala212
  val io = "org.scala-sbt" %% "io" % "1.4.0"
  val bloopFrontend = "ch.epfl.scala" %% "bloop-frontend" % "1.4.5"
}
