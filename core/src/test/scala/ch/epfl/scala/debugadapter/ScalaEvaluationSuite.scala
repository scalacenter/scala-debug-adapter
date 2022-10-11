package ch.epfl.scala.debugadapter

import utest._
import com.microsoft.java.debug.core.protocol.Types.Message
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import scala.concurrent.duration._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import java.nio.file.Path

abstract class ScalaEvaluationSuite(scalaVersion: ScalaVersion) extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  val executorService = Executors.newFixedThreadPool(1)
  implicit val ec =
    ExecutionContext.fromExecutorService(executorService)

  val isScala32 = scalaVersion == ScalaVersion.`3.2`
  val isScala3 = scalaVersion.binaryVersion.startsWith("3")
  val isScala2 = scalaVersion.binaryVersion.startsWith("2")
  val isScala213 = scalaVersion.binaryVersion == "2.13"
  val isScala212 = scalaVersion.binaryVersion == "2.12"

  case class Breakpoint(
      sourceFile: Option[Path],
      line: Int,
      condition: Option[String],
      ignore: Boolean,
      evaluations: Seq[Evaluation]
  )

  object Breakpoint {
    def apply(line: Int, ignore: Boolean = false)(evaluations: Evaluation*): Breakpoint =
      Breakpoint(None, line, None, ignore, evaluations)

    def apply(sourceFile: Path, line: Int)(evaluations: Evaluation*): Breakpoint =
      Breakpoint(Some(sourceFile), line, None, false, evaluations)

    def apply(line: Int, condition: String)(evaluations: Evaluation*): Breakpoint =
      Breakpoint(None, line, Some(condition), false, evaluations)
  }

  class Evaluation(
      val expression: String,
      val assertion: Either[Message, String] => Unit
  )

  object Evaluation {
    def failed(expression: String)(assertion: Message => Boolean) = {
      new Evaluation(
        expression,
        resp => assert(resp.left.exists(assertion))
      )
    }

    def failedOrIgnore(expression: String, ignore: Boolean)(
        assertion: Message => Boolean
    ) = {
      if (ignore) this.ignore(expression, new Message(0, "???"))
      else failed(expression)(assertion)
    }

    def successOrIgnore(expression: String, result: Any, ignore: Boolean) = {
      if (ignore) this.ignore(expression, result)
      else success(expression, result)
    }

    def successOrIgnore(expression: String, ignore: Boolean)(
        assertion: String => Boolean
    ) = {
      if (ignore) this.ignore(expression, "???")
      else success(expression)(assertion)
    }

    def ignore(expression: String, expected: Any) = {
      new Evaluation(
        expression,
        { resp =>
          println(s"TODO fix in $scalaVersion")
          println(s"expected: $expected")
        }
      )
    }

    def success(expression: String, result: Any): Evaluation = {
      val assertion: Either[Message, String] => Unit = resp =>
        result match {
          case expected: String =>
            assert(resp == Right('"' + expected + '"'))
          case () =>
            if (isScala3) assert(resp.exists(_.endsWith("\"()\"")))
            else assert(resp == Right("<void value>"))
          case expected @ (_: Boolean | _: Byte | _: Char | _: Int | _: Long | _: Short) =>
            assert(resp == Right(expected.toString))
          case floating @ (_: Double | _: Float) =>
            val expected = String.format(
              "%f",
              floating.toString().toDouble: java.lang.Double
            )
            assert(resp == Right(expected))
          case expected =>
            assert(resp.exists(_.endsWith("\"" + expected + "\"")))
        }
      new Evaluation(expression, assertion)
    }

    def success(expression: String)(assertion: String => Boolean) = {
      new Evaluation(
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
        new Evaluation(expression, resp => assert(assertion(resp)))
      )
    )
  }

  def assertInMainClass(sources: Seq[(String, String)], mainClass: String)(breakpoints: Breakpoint*): Unit = {
    val debuggee = MainDebuggee.mainClassRunner(sources, mainClass, scalaVersion, Seq.empty, Seq.empty)
    assertEvaluations(debuggee, breakpoints)
  }

  def assertInMainClass(source: String, mainClass: String)(breakpoints: Breakpoint*): Unit = {
    val debuggee =
      MainDebuggee.mainClassRunner(source, mainClass, scalaVersion)
    assertEvaluations(debuggee, breakpoints)
  }

  def assertInDebuggee(debuggee: MainDebuggee)(breakpoints: Breakpoint*): Unit = {
    assertEvaluations(debuggee, breakpoints)
  }

  def assertInMainClass(source: String, mainClass: String, scalacOptions: Seq[String])(
      breakpoints: Breakpoint*
  ): Unit = {
    val debuggee =
      MainDebuggee.mainClassRunner(source, mainClass, scalaVersion, scalacOptions)
    assertEvaluations(debuggee, breakpoints)
  }

  def assertInTestSuite(source: String, testSuite: String)(
      breakpoints: Breakpoint*
  ): Unit = {
    val debuggee =
      MainDebuggee.munitTestSuite(source, testSuite, scalaVersion)
    assertEvaluations(debuggee, breakpoints)
  }

  private def assertEvaluations(debuggee: MainDebuggee, allBreakpoints: Seq[Breakpoint]): Unit = {
    val tools = DebugTools(debuggee, ScalaInstanceResolver, NoopLogger)
    val server = DebugServer(debuggee, tools, NoopLogger, testMode = true)
    val client = TestDebugClient.connect(server.uri, 20.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val breakpoints = allBreakpoints.filter(!_.ignore)
      breakpoints
        .groupBy(_.sourceFile)
        .foreach { case (sourceOpt, breakpoints) =>
          val sourceFile = sourceOpt.getOrElse(debuggee.sourceFiles.head)
          val conditionalBreakpoints = breakpoints
            .map(b => (b.line, b.condition.orNull))
            .distinct
          val configuredBreakpoints = client.setConditionalBreakpoints(sourceFile, conditionalBreakpoints)
          assert(configuredBreakpoints.length == conditionalBreakpoints.length)
          assert(configuredBreakpoints.forall(_.verified))
        }
      client.configurationDone()

      breakpoints.foreach { breakpoint =>
        val stopped = client.stopped()
        val threadId = stopped.threadId
        assert(stopped.reason == "breakpoint")

        val stackTrace = client.stackTrace(threadId)
        val topFrame = stackTrace.stackFrames.head
        assert(topFrame.line == breakpoint.line)

        breakpoint.evaluations.foreach { evaluation =>
          val result = client.evaluate(evaluation.expression, topFrame.id)
          println(s"$$ ${evaluation.expression}")
          println("> " + result.left.map(_.format).merge)
          evaluation.assertion(result)
        }
        client.continue(threadId)
      }

      // This is flaky, terminated can happen before exited
      if (!GithubUtils.isCI()) {
        client.exited()
        client.terminated()
      }
    } finally {
      server.close()
      client.close()
    }
  }
}
