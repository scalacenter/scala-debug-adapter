import scala.util.Properties
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import java.nio.file.Paths

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

val jdkHome = Paths.get(Properties.jdkHome)

scalaVersion := "2.12.14"
javaHome := Some(jdkHome.toFile)
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath
  val logger = streams.value.log

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()
    client.configurationDone()
    client.outputedLine("com.sun.jdi.Value")
    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
