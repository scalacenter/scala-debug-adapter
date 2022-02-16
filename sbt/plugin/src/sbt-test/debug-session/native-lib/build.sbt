import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import com.microsoft.java.debug.core.protocol.Events.OutputEvent.Category

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "org.lmdbjava" % "lmdbjava" % "0.8.2"
scalaVersion := "2.12.15"

checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()
    client.configurationDone()

    val outputEvent = client.outputed(_.category == Category.stdout)
    assert(outputEvent.output == s"Success${System.lineSeparator}")

    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
