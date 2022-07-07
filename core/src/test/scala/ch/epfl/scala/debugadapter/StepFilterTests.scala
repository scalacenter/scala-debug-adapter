package ch.epfl.scala.debugadapter

import utest._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import scala.concurrent.duration._

object StepFilterTests extends TestSuite {
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
           |trait A {
           |  def m(): String = "A.m()"
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val b = new B
           |    println(b.m())
           |    val c = new C
           |    println(c.m())
           |    val d = new D
           |    println(d.m())
           |    val e = new E
           |    println(e.m())
           |    println(F.m())
           |    val g = new G
           |    println(g.m())
           |    val h = new H
           |    println(h.m())
           |    val a1 = new A {}
           |    println(a1.m())
           |    val a2 = new A {
           |      override def m(): String = "g.m()"
           |    }
           |    println(a2.m())
           |  }
           |
           |  private class G extends A {
           |    override def m(): String = "G.m()"
           |  }
           |
           |  class H extends A
           |}
           |
           |class B extends A
           |
           |class C extends A {
           |  def m(x: Int): String = s"C.m($x)"
           |}
           |
           |class D extends A {
           |  override def m(): String = "D.m()"
           |}
           |
           |class E extends D
           |
           |object F extends A
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(10)(StepInto(4)),
        Breakpoint(12)(StepInto(4)),
        Breakpoint(14)(StepInto(44)),
        Breakpoint(16)(StepInto(44)),
        Breakpoint(17)(StepInto(4)),
        Breakpoint(19)(StepInto(31)),
        Breakpoint(21)(StepInto(4)),
        // cannot skip method in local class
        Breakpoint(23)(StepInto(22)),
        Breakpoint(27)(StepInto(25))
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
      val resp = client.setBreakpoints(runner.sourceFiles.head, lines)
      assert(resp.length == lines.length)
      assert(resp.forall(_.verified))
      client.configurationDone()

      breakpoints.foreach { breakpoint =>
        val stopped = client.stopped()
        assert(stopped.reason == "breakpoint")
        val threadId = stopped.threadId
        val stackTrace = client.stackTrace(threadId)
        assert(stackTrace.stackFrames.head.line == breakpoint.line)

        breakpoint.steps.foreach { step =>
          println(s"\nStepping into, at line ${breakpoint.line}")
          client.stepIn(threadId)
          client.stopped(300.seconds)
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
