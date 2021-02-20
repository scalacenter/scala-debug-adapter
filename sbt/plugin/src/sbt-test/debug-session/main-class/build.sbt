import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

val scala3   = "3.0.0-RC1"
val scala213 = "2.13.4"
val scala212 = "2.12.13"
val scala211 = "2.11.12"
val supportedScalaVersions = List(scala3, scala213, scala212, scala211)

scalaVersion := scala212
crossScalaVersions := supportedScalaVersions

checkDebugSession := {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (Compile / startMainClassDebugSession).evaluated
  val source = (Compile / sources).value.head.toPath

  val client = TestDebugClient.connect(uri)
  try {
    client.initialize()
    client.launch()
    
    val breakpoints = client.setBreakpoints(source, Array(3, 5, 9))
    assert(breakpoints.size == 3)
    assert(breakpoints.forall(_.verified))

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
