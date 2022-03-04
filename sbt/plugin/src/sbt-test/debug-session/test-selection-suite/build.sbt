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
    val outputEvent = client.outputed(_.category == Category.stdout)

    // Xmx is set to 1G
    // 1GB = 1024 * 1024 * 1024 = 1073741824
    // env variable KEY is set to the VALUE
    assert(outputEvent.output.trim == "test1_1073741824_VALUE")

    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
