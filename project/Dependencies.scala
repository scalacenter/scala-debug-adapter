import sbt._

object Dependencies {
  val scala212 = "2.12.16"
  val scala213 = "2.13.9"
  val scala3 = "3.2.0"
  val asmVersion = "9.3"
  val coursierVersion = "2.1.0-M6"

  val asm = "org.ow2.asm" % "asm" % asmVersion
  val asmUtil = "org.ow2.asm" % "asm-util" % asmVersion
  val javaDebug =
    "ch.epfl.scala" % "com-microsoft-java-debug-core" % "0.34.0+4"
  val utest = "com.lihaoyi" %% "utest" % "0.7.10"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scala212
  val scalaReflect = "org.scala-lang" % "scala-reflect" % scala212
  val sbtIo = "org.scala-sbt" %% "io" % "1.5.1"
  val sbtTestInterface = "org.scala-sbt" % "test-interface" % "1.0"
  val sbtTestAgent = "org.scala-sbt" % "test-agent" % "1.7.1"
  val coursier = "io.get-coursier" %% "coursier" % coursierVersion
  val coursierJvm = "io.get-coursier" %% "coursier-jvm" % coursierVersion
  val pprint = "com.lihaoyi" %% "pprint" % "0.7.3"
}
