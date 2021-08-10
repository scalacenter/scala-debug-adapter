package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import sbt.io.IO
import utest._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object Scala212DebugTest extends ScalaDebugTestSuite(ScalaVersion.`2.12`)
object Scala213DebugTest extends ScalaDebugTestSuite(ScalaVersion.`2.13`)
object Scala3DebugTest extends ScalaDebugTestSuite(ScalaVersion.`3`)

class ScalaDebugTestSuite(scalaVersion: ScalaVersion) extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService  = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def tests: Tests = Tests {
    "should support breakpoints in scala sources" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, scalaVersion)
      val server = DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()
        
        val breakpoints = client.setBreakpoints(runner.source, Array(5, 13, 20, 14, 9))
        assert(breakpoints.size == 5)
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
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should support breakpoints in fully qualified classes" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, scalaVersion)
      val server = DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()
        
        val breakpoints = client.setBreakpoints(runner.mainClass, Array(5, 9))
        assert(breakpoints.size == 2)
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
        client.exited
        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should return stacktrace, scopes and variables when stopped by a breakpoint" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, scalaVersion)
      val server = DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()
        client.setBreakpoints(runner.source, Array(7))
        client.configurationDone()

        val stopped = client.stopped
        val stackTrace = client.stackTrace(stopped.threadId)
        assert(stackTrace.totalFrames == 2)
        
        val topFrame = stackTrace.stackFrames.head
        val scopes = client.scopes(topFrame.id)
        assert(scopes.length == 1)

        val localScope = scopes.head
        assert(localScope.name == "Local")

        val localVars = client.variables(localScope.variablesReference)
        assertMatch(localVars.map(_.name)) {
          case Array("args", "h", "this") => ()
        }

        client.continue(stopped.threadId)
        client.exited
        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }
  }
}
