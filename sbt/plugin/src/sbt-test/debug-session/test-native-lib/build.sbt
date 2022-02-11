import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the test suite debug session")

libraryDependencies += "org.lmdbjava" % "lmdbjava" % "0.8.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
scalaVersion := "2.12.15"

checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Test / startTestSuitesDebugSession).evaluated
  val specSource = (Test / sources).value.head.toPath

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()

    val breakpoints = client.setBreakpoints(specSource, Array(10, 15))
    assert(breakpoints.size == 2)
    assert(breakpoints.forall(_.verified))

    client.configurationDone()

    val threadId = client.stopped().threadId

    client.continue(threadId)
    client.stopped()

    client.continue(threadId)
    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
