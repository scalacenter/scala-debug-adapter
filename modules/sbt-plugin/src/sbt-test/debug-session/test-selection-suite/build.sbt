import ch.epfl.scala.debugadapter.testfmk._
import munit.Assertions._

libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
testFrameworks += TestFrameworks.JUnit
scalaVersion := "2.12.14"

Test / fork := true

InputKey[Unit]("checkDebugSession") := {
  val uri = (Test / startTestSuitesSelectionDebugSession).evaluated

  // Xmx is set to 1G
  // 1GB = 1024 * 1024 * 1024 = 1073741824
  // env variable KEY is set to the VALUE

  // for some reason jdk8 yields 954728448 for Xmx1G
  val isJava8 = System.getProperty("java.version").startsWith("1.8.")
  val expected = if (isJava8) "test1_954728448_VALUE" else "test1_1073741824_VALUE"

  DebugTest.check(uri)(
    Outputed(msg => assert(msg.contains("test2"))),
    Outputed(expected)
  )
}
