package ch.epfl.scala.debugadapter

import utest._
import com.microsoft.java.debug.core.protocol.Types.Message
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import scala.concurrent.duration._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

abstract class ScalaEvaluationSuite(scalaVersion: ScalaVersion)
    extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec =
    ExecutionContext.fromExecutorService(executorService)

  val isScala31 = scalaVersion == ScalaVersion.`3.1`
  val isScala3 = scalaVersion.binaryVersion.startsWith("3")

  class Breakpoint(val line: Int, val evaluations: Seq[ExpressionEvaluation])

  object Breakpoint {
    def apply(line: Int)(evaluations: ExpressionEvaluation*): Breakpoint = {
      new Breakpoint(line, evaluations)
    }
  }

  class ExpressionEvaluation(
      val expression: String,
      val assertion: Either[Message, String] => Unit
  )

  object ExpressionEvaluation {
    def failed(expression: String)(assertion: Message => Boolean) = {
      new ExpressionEvaluation(
        expression,
        resp => assert(resp.left.exists(assertion))
      )
    }

    def success(expression: String, result: Any) = {
      result match {
        case str: String =>
          new ExpressionEvaluation(
            expression,
            resp => assert(resp == Right('"' + str + '"'))
          )
        case () if isScala3 =>
          new ExpressionEvaluation(
            expression,
            resp => assert(resp.exists(_.endsWith("\"()\"")))
          )
        case () =>
          new ExpressionEvaluation(
            expression,
            resp => assert(resp == Right("<void value>"))
          )
        case n: Int =>
          new ExpressionEvaluation(
            expression,
            resp =>
              assertMatch(resp) {
                case Right(m) if m == n.toString => ()
                case Right(m: String) if m.endsWith('"' + n.toString + '"') =>
                  ()
              }
          )
        case _ =>
          new ExpressionEvaluation(
            expression,
            resp => assert(resp == Right(result.toString))
          )
      }
    }

    def success(expression: String)(assertion: String => Boolean) = {
      new ExpressionEvaluation(
        expression,
        resp => assert(resp.exists(assertion))
      )
    }
  }

  def assertInMainClass(
      source: String,
      mainClass: String,
      line: Int,
      expression: String
  )(assertion: Either[Message, String] => Boolean): Unit = {
    assertInMainClass(source, mainClass)(
      Breakpoint(line)(
        new ExpressionEvaluation(expression, resp => assert(assertion(resp)))
      )
    )
  }

  def assertInMainClass(source: String, mainClass: String)(
      breakpoints: Breakpoint*
  ): Unit = {
    val runner =
      MainDebuggeeRunner.mainClassRunner(source, mainClass, scalaVersion)
    assertEvaluations(runner, breakpoints)
  }

  def assertInTestSuite(source: String, testSuite: String)(
      breakpoints: Breakpoint*
  ): Unit = {
    val runner =
      MainDebuggeeRunner.munitTestSuite(source, testSuite, scalaVersion)
    assertEvaluations(runner, breakpoints)
  }

  private def assertEvaluations(
      runner: MainDebuggeeRunner,
      breakpoints: Seq[Breakpoint]
  ): Unit = {
    val server = DebugServer(runner, NoopLogger)
    val client = TestDebugClient.connect(server.uri, 20.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val lines = breakpoints.map(_.line).distinct.toArray
      val configuredBreakpoints = client.setBreakpoints(runner.source, lines)
      assert(configuredBreakpoints.length == lines.length)
      assert(configuredBreakpoints.forall(_.verified))
      client.configurationDone()

      breakpoints.foreach { breakpoint =>
        val stopped = client.stopped()
        val threadId = stopped.threadId
        assert(stopped.reason == "breakpoint")

        val stackTrace = client.stackTrace(threadId)
        val topFrame = stackTrace.stackFrames.head
        breakpoint.evaluations.foreach { evaluation =>
          val result = client.evaluate(evaluation.expression, topFrame.id)
          evaluation.assertion(result)
        }
        client.continue(threadId)
      }

      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
    }
  }
}
