import scala.concurrent.ExecutionContext
import ch.epfl.scala.debug.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % Test
testFrameworks += new TestFramework("utest.runner.Framework")
scalaVersion := "2.12.12"
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Test / startTestSuitesDebugSession).evaluated
  val mainSource = (Compile / sources).value.head.toPath
  val specSource = (Test / sources).value.head.toPath

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()
    
    client.setBreakpoints(mainSource, Array(3))
    client.setBreakpoints(specSource, Array(6, 8))
    client.configurationDone()

    val threadId = client.stopped.threadId
    
    client.continue(threadId)
    client.stopped
    
    client.continue(threadId)
    client.stopped

    client.continue(threadId)
    client.exited
    client.terminated
  } finally {
    client.close()
  }
}
