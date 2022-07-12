package ch.epfl.scala.debugadapter

import utest._
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import ch.epfl.scala.debugadapter.testing.TestDebugClient
import scala.concurrent.duration._

object Scala212StepFilterTests extends StepFilterSuite(ScalaVersion.`2.12`)
object Scala213StepFilterTests extends StepFilterSuite(ScalaVersion.`2.13`)

abstract class StepFilterSuite(scalaVersion: ScalaVersion) extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  private val executorService = Executors.newFixedThreadPool(1)
  private implicit val ec =
    ExecutionContext.fromExecutorService(executorService)

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

    "should not step into getters" - {
      val source =
        """|package example
           |
           |object Main {
           |  val x1 = "x1"
           |  private val x2 = "x2"
           |  var x3 = "x3"
           |  private var x4 = "x4"
           |
           |  def foo(x: String): Unit = {
           |    println(x)
           |  }
           |
           |  def main(args: Array[String]): Unit = {
           |    foo(x1)
           |    foo(x2)
           |    foo(x3)
           |    foo(x4)
           |
           |    val c = new C("c1", "c2")
           |    c.m()
           |
           |    val d = D("d1")
           |    foo(d.d1)
           |  }
           |}
           |
           |trait A {
           |  val a1: String
           |  def a2: String
           |}
           |
           |abstract class B {
           |  val b1: String = "b1"
           |  protected val b2: String = "b2"
           |}
           |
           |class C(val c1: String, c2: String) extends B with A {
           |  override val a1: String = "a1"
           |  override val a2: String = "a2"
           |  private val c3: String = "c3"
           |
           |  def m(): Unit = {
           |    Main.foo(a1)
           |    Main.foo(a2)
           |    Main.foo(b1)
           |    Main.foo(b2)
           |    Main.foo(c1)
           |    Main.foo(c2)
           |    Main.foo(c3)
           |  }
           |}
           |
           |case class D(d1: String)
           |
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(14)(StepInto(10)),
        Breakpoint(15)(StepInto(10)),
        Breakpoint(16)(StepInto(10)),
        Breakpoint(17)(StepInto(10)),
        Breakpoint(43)(StepInto(10)),
        Breakpoint(44)(StepInto(10)),
        Breakpoint(45)(StepInto(10)),
        Breakpoint(46)(StepInto(10)),
        Breakpoint(47)(StepInto(10)),
        Breakpoint(48)(StepInto(10)),
        Breakpoint(49)(StepInto(10)),
        Breakpoint(23)(StepInto(10))
      )
    }

    "should not step into setters" - {
      val source =
        """|package example
           |
           |object Main {
           |  var x1 = "x1"
           |  private var x2 = "x2"
           |  
           |  def main(args: Array[String]): Unit = {
           |    x1 = "x1"
           |    x2 = "x2"
           |    
           |    val c = new C("c1", "c2")
           |    c.m()
           |  }
           |}
           |
           |trait A {
           |  var a1: String
           |}
           |
           |abstract class B {
           |  var b1: String = "b1"
           |  protected var b2: String = "b2"
           |}
           |
           |class C(var c1: String, private var c2: String) extends B with A {
           |  override var a1: String = "a1"
           |  private var c3: String = "c3"
           |  
           |  def m(): Unit = {
           |    a1 = "a1"
           |    b1 = "b1"
           |    b2 = "b2"
           |    c1 = "c1"
           |    c2 = "c2"
           |    c3 = "c3"
           |  }
           |}
           |
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(8)(
          StepInto(9),
          StepInto(11)
        ),
        Breakpoint(30)(
          StepInto(31),
          StepInto(32),
          StepInto(33),
          StepInto(34),
          StepInto(35),
          StepInto(12)
        )
      )
    }

    "should not step into bridges" - {
      val source =
        """|package example
           |
           |class A {
           |  def m(): Object = "object"
           |}
           |
           |class B extends A {
           |  override def m(): String = "string"
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val b: A = new B
           |    println(b.m())
           |  }
           |}
           |
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(14)(StepInto(8))
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

        var currentLine = breakpoint.line
        breakpoint.steps.foreach { step =>
          println(s"\nStepping into, at line $currentLine")
          client.stepIn(threadId)
          client.stopped(300.seconds)
          val stackTrace = client.stackTrace(threadId)
          val obtained = stackTrace.stackFrames.head.line
          val expected = step.line
          assert(obtained == expected)
          currentLine == obtained
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
