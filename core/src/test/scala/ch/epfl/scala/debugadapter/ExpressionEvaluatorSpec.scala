package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import sbt.io.IO
import utest._

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import com.microsoft.java.debug.core.protocol.Types.Message

object ExpressionEvaluatorSpec extends TestSuite {
  // the server needs only one thread for delayed responses of the launch and configurationDone requests
  private val executorService = Executors.newFixedThreadPool(1)
  private implicit val ec =
    ExecutionContext.fromExecutorService(executorService)

  def tests: Tests = Tests {
    "should evaluate expression with primitives" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 3, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "should evaluate expression with local variables" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val x1 = 1.1
          |    val x2 = 2.2
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 5, "x1 + x2")(
        _.exists(_.toDouble == 3.3)
      )
    }

    "should evaluate expression with object's public fields" - {
      val source =
        """object A {
          |  val x1 = "x1"
          |}
          |
          |object EvaluateTest {
          |  val x1 = 1.1
          |  val x2 = 2.2
          |
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 10, "x1 + x2")(
        _.exists(_.toDouble == 3.3)
      )
      assertEvaluation(source, "EvaluateTest", 10, "A.x1")(
        _.exists(_ == "\"x1\"")
      )
    }

    "should evaluate expression with object's private fields" - {
      val source =
        """object EvaluateTest {
          |  private val x1 = 1.1
          |  private val x2 = 2.2
          |
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 6, "x1 + x2")(
        _.exists(_.toDouble == 3.3)
      )
    }

    "should evaluate expression with class's public fields" - {
      val source =
        """class A {
          |  val x1 = "x1"
          |
          |  def m1(): Unit = {
          |    println("m1")
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new A().m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 5, "x1")(_.exists(_ == "\"x1\""))
      assertEvaluation(source, "EvaluateTest", 11, "new A().x1")(
        _.exists(_ == "\"x1\"")
      )
    }

    "should evaluate expression with class's private fields" - {
      val source =
        """class A {
          |  private val x1 = "x1"
          |
          |  def m1(): Unit = {
          |    println("m1")
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new A().m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 5, "x1")(_.exists(_ == "\"x1\""))
    }

    "should evaluate expression with inner class's public fields" - {
      val source =
        """class A {
          |  class B {
          |    val x1 = "x1"
          |
          |    def m1(): Unit = {
          |      println("m1")
          |    }
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = new A()
          |    val b = new a.B()
          |    b.m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 6, "x1")(_.exists(_ == "\"x1\""))
      assertEvaluation(source, "EvaluateTest", 15, "b.x1")(
        _.exists(_ == "\"x1\"")
      )
    }

    "should evaluate expression with inner class's private fields" - {
      val source =
        """class A {
          |  class B {
          |    private val x1 = "x1"
          |
          |    def m1(): Unit = {
          |      println("m1")
          |    }
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = new A()
          |    val b = new a.B()
          |    b.m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 6, "x1")(_.exists(_ == "\"x1\""))
    }

    "should evaluate expression with outer class's public fields" - {
      val source =
        """class A {
          |  val x1 = "x1"
          |  class B {
          |    def m1(): Unit = {
          |      println("m1")
          |    }
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = new A()
          |    val b = new a.B()
          |    b.m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 5, "x1")(_.exists(_ == "\"x1\""))
    }

    "should evaluate expression with a public method call" - {
      val source =
        """class B
          |class A {
          |  val x1 = new B()
          |  def m1(): Unit = {
          |    println("m1")
          |  }
          |
          |  def m2(): Int = {
          |    1
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = new A()
          |    a.m1()
          |  }
          |}
          |""".stripMargin

      assertEvaluation(source, "EvaluateTest", 5, "m2()")(
        _.exists(_.toInt == 1)
      )
    }

    "should evaluate expression with inner class's overridden fields" - {
      val source =
        """class A {
          |  val x1 = "x1"
          |  class B {
          |    val x1 = "x1x1"
          |
          |    def m1(): Unit = {
          |      println("m1")
          |    }
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = new A()
          |    val b = new a.B()
          |    b.m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 7, "x1")(
        _.exists(_ == "\"x1x1\"")
      )
    }

    "should evaluate expression in package" - {
      val source =
        """package debug {
          |object EvaluateTest {
          |    def main(args: Array[String]): Unit = {
          |      println("Hello, World!")
          |    }
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "debug.EvaluateTest", 4, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "should evaluate expression with Java util code" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluation(
        source,
        "EvaluateTest",
        3,
        "new java.util.ArrayList[String]().toString"
      )(_.exists(_ == "\"[]\""))
    }

    "should return null when expression is invalid" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 3, "1 ++ 2")(
        _.left.exists(_.format == "value ++ is not a member of Int")
      )
    }

    "should evaluate expression inside of a lambda" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    List(1).foreach(n => {
          |      println(n)
          |    })
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 4, "n")(_.exists(_.toInt == 1))
    }

    "should evaluate expression a object's method call inside of a lambda" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    List(1).foreach(n => {
          |      println(n)
          |    })
          |  }
          |
          |  def m1(): Int = 9
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 4, "m1()")(
        _.exists(_.toInt == 9)
      )
    }

    "should evaluate expression a class's method call inside of a lambda" - {
      val source =
        """class A {
          |  def m1(): Unit = {
          |    List(1).foreach(n => {
          |      println(n)
          |      println(m2())
          |    })
          |  }
          |
          |  def m2(): Int = 10
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new A().m1()
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "EvaluateTest", 4, "m2()")(
        _.exists(_.toInt == 10)
      )
    }

    "should not loop indefinitely" - {
      val source =
        """package test
          |
          |object EvaluateTest extends App {
          |  m()
          |
          |  def m(): Unit = {
          |    val a = List(1, 2)
          |  }
          |}
          |""".stripMargin
      assertEvaluation(source, "test.EvaluateTest", 7, "\"Hello\"") { res =>
        res.left.exists(_.format.startsWith("Compilation timed out"))
      }
    }
  }

  private def assertEvaluation(
      source: String,
      mainClass: String,
      line: Int,
      expression: String
  )(assertion: Either[Message, String] => Boolean): Unit = {
    val tempDir = IO.createTemporaryDirectory
    val srcDir = new File(tempDir, "src")
    IO.createDirectory(srcDir)
    val outDir = new File(tempDir, "out")
    IO.createDirectory(outDir)

    val runner = MainDebuggeeRunner.fromSource(
      srcDir,
      "EvaluateTest.scala",
      source,
      mainClass,
      outDir
    )
    val server = DebugServer(runner, NoopLogger)
    val client = TestDebugClient.connect(server.uri, 20.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val breakpoints = client.setBreakpoints(runner.source, Array(line))
      assert(breakpoints.length == 1)
      assert(breakpoints.forall(_.verified))
      client.configurationDone()

      val stopped = client.stopped()
      val threadId = stopped.threadId
      assert(stopped.reason == "breakpoint")

      val stackTrace = client.stackTrace(threadId)
      val topFrame = stackTrace.stackFrames.head

      val result = client.evaluate(expression, topFrame.id)
      assert(assertion(result))

      client.continue(threadId)
      client.exited()
      client.terminated()
    } finally {
      server.close()
      client.close()
      IO.delete(tempDir)
    }
  }
}
