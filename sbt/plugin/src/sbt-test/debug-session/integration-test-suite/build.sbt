import ch.epfl.scala.debug.DebugAdapterPlugin
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debug.testing.TestDebugClient

val checkDebugSession = inputKey[Unit]("Check the integration test suite debug session")

val root = project.in(file("."))
  .configs(IntegrationTest)
  .settings(
    scalaVersion := "2.12.12",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % IntegrationTest,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Defaults.itSettings,
    inConfig(IntegrationTest)(DebugAdapterPlugin.testSettings),
    checkDebugSession := checkDebugSessionTask.evaluated
  )

def checkDebugSessionTask = Def.inputTask {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val uri = (IntegrationTest / startTestSuitesDebugSession).evaluated
  val mainSource = (Compile / sources).value.head.toPath
  val specSource = (IntegrationTest / sources).value.head.toPath

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