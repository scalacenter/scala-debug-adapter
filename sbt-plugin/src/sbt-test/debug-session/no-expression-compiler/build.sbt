import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

// there is no expression compiler for Scala 2.11
scalaVersion := "2.11.12"

// check that one can start a debug session even if the expression compiler
// cannot be resolved
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()

    val breakpoints = client.setBreakpoints(source, Array(4))
    assert(breakpoints.size == 1)
    assert(breakpoints.forall(_.verified))

    client.configurationDone()

    val stopped = client.stopped()
    assert(stopped.reason == "breakpoint")

    val stackTrace = client.stackTrace(stopped.threadId)
    val topFrame = stackTrace.stackFrames.head

    val error = client.evaluate("1 + 1", topFrame.id).left
    assert(error.exists(_.format.contains("Missing evaluator for entry")))

    client.continue(stopped.threadId)
    client.exited()
    client.terminated()
  } finally {
    client.close()
  }
}
