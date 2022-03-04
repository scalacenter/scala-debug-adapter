import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import com.microsoft.java.debug.core.protocol.Events.OutputEvent.Category

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
testFrameworks += TestFrameworks.JUnit
scalaVersion := "2.12.14"

Test / fork := true

checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Test / startTestSuitesSelectionDebugSession).evaluated

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()

    client.configurationDone()

    val ignoredTest = client.outputed(_.category == Category.stdout)

    assert(ignoredTest.output.contains("test2"))

    val outputEvent = client.outputed(_.category == Category.stdout)

    // Xmx is set to 1G
    // 1GB = 1024 * 1024 * 1024 = 1073741824
    // env variable KEY is set to the VALUE

    // for some reason jdk8 yields 954728448 for Xmx1G
    val isJava8 = System.getProperty("java.version").startsWith("1.8.")
    val expected =
      if (isJava8) "test1_954728448_VALUE"
      else "test1_1073741824_VALUE"
    val obtained = outputEvent.output.trim

    assert(obtained == expected, s"got: $obtained\nexpected: $expected")

    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
