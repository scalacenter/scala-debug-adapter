import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import ch.epfl.scala.debug.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

scalaVersion := "2.12.12"
checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()
    
    val breakpoints = client.setBreakpoints(source.toPath, Array(3, 11, 18, 12, 7))
    assert(breakpoints.forall(_.verified))
    
    client.configurationDone()
    val stopped1 = client.stopped
    val threadId = stopped1.threadId
    assert(stopped1.reason == "breakpoint")
    
    client.continue(threadId)
    val stopped2 = client.stopped
    assert(stopped2.reason == "breakpoint")
    assert(stopped2.threadId == threadId)
    
    client.continue(threadId)
    val stopped3 = client.stopped
    assert(stopped3.reason == "breakpoint")
    assert(stopped3.threadId == threadId)
    
    client.continue(threadId)
    val stopped4 = client.stopped
    assert(stopped4.reason == "breakpoint")
    assert(stopped4.threadId == threadId)
    
    client.continue(threadId)
    val stopped5 = client.stopped
    assert(stopped5.reason == "breakpoint")
    assert(stopped5.threadId == threadId)

    client.continue(threadId)
    client.exited
    client.terminated
  } finally {
    client.close()
  }
}
