package ch.epfl.scala.debugadapter

import utest._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import scala.concurrent.duration._

object SmartStepTests extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  private val executorService = Executors.newFixedThreadPool(1)
  private implicit val ec =
    ExecutionContext.fromExecutorService(executorService)
  private val scalaVersion = ScalaVersion.`2.12`

  def tests: Tests = Tests {
    "should not step into mixin forwarder" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val b = new B()
           |    println(b.foo)
           |  }
           |}
           |
           |trait A {
           |  def foo: String = "foo"
           |}
           |
           |class B() extends A
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        // at a breakpoint on line 6, it should step into line 11
        Breakpoint(6)(StepInto(11))
      )
    }
  }

  case class Breakpoint(line: Int)(val steps: StepInto*)
  case class StepInto(line: Int)

  private def assertInMainClass(
      source: String,
      mainClass: String
  )(breakpoints: Breakpoint*): Unit = {
    val runner =
      MainDebuggeeRunner.mainClassRunner(source, mainClass, scalaVersion)
    val server = DebugServer(runner, NoopLogger)
    val client = TestDebugClient.connect(server.uri, 20.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val lines = breakpoints.map(_.line).distinct.toArray
      val resp = client.setBreakpoints(runner.source, lines)
      assert(resp.length == lines.length)
      assert(resp.forall(_.verified))
      client.configurationDone()

      breakpoints.foreach { breakpoint =>
        val stopped = client.stopped()
        val threadId = stopped.threadId
        assert(stopped.reason == "breakpoint")

        breakpoint.steps.foreach { step =>
          client.stepIn(threadId)
          client.stopped(4.seconds)
          val stackTrace = client.stackTrace(threadId)
          val obtained = stackTrace.stackFrames.head.line
          val expected = step.line
          assert(obtained == expected)
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
