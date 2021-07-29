package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import sbt.io.IO
import utest._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object ExpressionEvaluatorSpec extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec = ExecutionContext.fromExecutorService(executorService)

  def tests: Tests = Tests {
    "should evaluate expression with primitives" - {
      assertEvaluation(163, "1 + 2", _.toInt == 3)
    }

    "should evaluate expression with local variables" - {
      assertEvaluation(165, "a + b", _.toDouble == 3.3)
    }

    "should evaluate expression with object's public fields" - {
      assertEvaluation(88, "x1 + x2", _.toInt == 43)
    }

    "should evaluate expression with object's private fields" - {
      assertEvaluation(88, "y1 + y2", _.toInt == 47)
    }

    "should evaluate expression with class's public fields" - {
      assertEvaluation(8, "x1 + x2", _.toInt == 3)
    }

    "should evaluate expression with class's private fields" - {
      assertEvaluation(8, "y2", _ == "\"foo\"")
    }

    "should evaluate expression with inner class's public fields" - {
      assertEvaluation(23, "2 * z1", _.toInt == 12)
    }

    "should evaluate expression with inner class's private fields" - {
      assertEvaluation(23, "y1 + y2", _.toInt == 15)
    }

    "should evaluate expression with outer class's public fields" - {
      assertEvaluation(23, "2 * x2", _.toInt == 4)
    }

    "should evaluate expression with inner class's overridden fields" - {
      assertEvaluation(23, "x1", _ == "\"foo\"")
    }
  }

  private def assertEvaluation(line: Int, expression: String, assertion: String => Boolean): Unit = {
    val tempDir = IO.createTemporaryDirectory
    val runner = MainDebuggeeRunner.evaluateTest(tempDir)
    val server = DebugServer(runner, NoopLogger)
    val client = TestDebugClient.connect(server.uri, 10.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val breakpoints = client.setBreakpoints(runner.source, Array(line))
      assert(breakpoints.length == 1)
      assert(breakpoints.forall(_.verified))
      client.configurationDone()

      val stopped = client.stopped
      val threadId = stopped.threadId
      assert(stopped.reason == "breakpoint")

      val stackTrace = client.stackTrace(threadId)
      val topFrame = stackTrace.stackFrames.head

      val result = client.evaluate(expression, topFrame.id)
      assert(assertion(result))

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
