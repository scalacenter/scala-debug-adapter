import scala.util.Properties
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import java.nio.file.Paths

val checkDebugSession = inputKey[Unit]("Check the main class debug session")

val jdkHome = Paths.get(Properties.jdkHome)
val jdkSourceZip = jdkHome.resolve("src.zip")

scalaVersion := "2.12.12"
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
    
    val breakpoints = client.setBreakpoints(source, Array(5))
    assert(breakpoints.size == 1)
    assert(breakpoints.forall(_.verified))

    client.configurationDone()
    
    val threadId = client.stopped.threadId

    client.stepIn(threadId)
    client.stopped

    val frame = client.stackTrace(threadId).stackFrames.head
    
    val javaVersion = Properties.javaVersion
    if (javaVersion.startsWith("1.")) {
      logger.info(s"Java version is: $javaVersion")
      val expectedSource = s"jar:${jdkSourceZip.toUri}!/java/io/PrintStream.java"
      assert(frame.source.path == expectedSource)
    } else {
      logger.info(s"Skipping test for java version: $javaVersion")
    }

    client.continue(threadId)

    client.exited
    client.terminated
  } finally {
    client.close()
  }
}
