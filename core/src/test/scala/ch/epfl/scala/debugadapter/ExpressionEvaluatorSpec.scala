package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import sbt.io.IO
import utest._

import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object ExpressionEvaluatorSpec extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def tests: Tests = Tests {
    "should evaluate scala expression" - {
      val tempDir = IO.createTemporaryDirectory
      val runner = MainDebuggeeRunner.evaluateTest(tempDir)
      val server = DebugServer(runner, NoopLogger)
      val client = TestDebugClient.connect(server.uri)
      try {
        server.connect()
        client.initialize()
        client.launch()

        val breakpoints = client.setBreakpoints(runner.source, Array(15))
        assert(breakpoints.length == 1)
        assert(breakpoints.forall(_.verified))
        client.configurationDone()

        val stopped = client.stopped
        val threadId = stopped.threadId
        assert(stopped.reason == "breakpoint")

        val stackTrace = client.stackTrace(threadId)
        val topFrame = stackTrace.stackFrames.head

        println(client.evaluate("a", topFrame.id))
        println(client.evaluate("b", topFrame.id))
        println(client.evaluate("c", topFrame.id))
        println(client.evaluate("args", topFrame.id))
        println(client.evaluate("y", topFrame.id))
        println(client.evaluate("z", topFrame.id))

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
