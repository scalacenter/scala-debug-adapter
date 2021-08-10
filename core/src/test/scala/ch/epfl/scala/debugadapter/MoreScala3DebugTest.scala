package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import sbt.io.IO
import utest._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object MoreScala3DebugTest extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService  = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def tests: Tests = Tests {
    "should support breakpoints in scala 3 with brace-less syntax" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scala3Braceless(tempDir)
      val server = DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()
        
        val breakpoints = client.setBreakpoints(runner.source, Array(5, 7, 11))
        assert(breakpoints.size == 3)
        assert(breakpoints.forall(_.verified))
        
        client.configurationDone()
        val stop = client.stopped
        val threadId = stop.threadId
        
        client.continue(threadId)
        client.stopped

        client.continue(threadId)
        client.stopped

        client.continue(threadId)
        client.exited
        client.terminated
      } finally {
        server.close()
        client.close()
        IO.delete(tempDir)
      }
    }

    "should support breakpoints in scala 3 with @main" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.scala3MainAnnotation(tempDir)
      val server = DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()
        
        val breakpoints = client.setBreakpoints(runner.source, Array(4, 6, 10))
        assert(breakpoints.size == 3)
        assert(breakpoints.forall(_.verified))
        
        client.configurationDone()
        val stop = client.stopped
        val threadId = stop.threadId
        
        client.continue(threadId)
        client.stopped

        client.continue(threadId)
        client.stopped

        client.continue(threadId)
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
