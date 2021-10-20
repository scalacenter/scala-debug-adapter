package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testing.TestDebugClient
import com.microsoft.java.debug.core.protocol.Types.Message
import utest._

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Scala212EvaluatorSpec
    extends ExpressionEvaluatorSuite(ScalaVersion.`2.12`)
object Scala213EvaluatorSpec
    extends ExpressionEvaluatorSuite(ScalaVersion.`2.13`)

abstract class ExpressionEvaluatorSuite(scalaVersion: ScalaVersion)
    extends TestSuite {
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
      assertEvaluationInMainClass(source, "EvaluateTest", 3, "1 + 2")(
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
      assertEvaluationInMainClass(source, "EvaluateTest", 5, "x1 + x2")(
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
      assertEvaluationInMainClass(source, "EvaluateTest", 10, "x1 + x2")(
        _.exists(_.toDouble == 3.3)
      )
      assertEvaluationInMainClass(source, "EvaluateTest", 10, "A.x1")(
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
      assertEvaluationInMainClass(source, "EvaluateTest", 6, "x1 + x2")(
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
      assertEvaluationsInMainClass(
        source,
        "EvaluateTest",
        ExpressionEvaluation(11, "new A().x1", _.exists(_ == "\"x1\"")),
        ExpressionEvaluation(5, "x1", _.exists(_ == "\"x1\""))
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
      assertEvaluationInMainClass(source, "EvaluateTest", 5, "x1")(
        _.exists(_ == "\"x1\"")
      )
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
      assertEvaluationsInMainClass(
        source,
        "EvaluateTest",
        ExpressionEvaluation(15, "b.x1", _.exists(_ == "\"x1\"")),
        ExpressionEvaluation(6, "x1", _.exists(_ == "\"x1\""))
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
      assertEvaluationInMainClass(source, "EvaluateTest", 6, "x1")(
        _.exists(_ == "\"x1\"")
      )
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
      assertEvaluationInMainClass(source, "EvaluateTest", 5, "x1")(
        _.exists(_ == "\"x1\"")
      )
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

      assertEvaluationInMainClass(source, "EvaluateTest", 5, "m2()")(
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
      assertEvaluationInMainClass(source, "EvaluateTest", 7, "x1")(
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
      assertEvaluationInMainClass(source, "debug.EvaluateTest", 4, "1 + 2")(
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
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        3,
        "new java.util.ArrayList[String]().toString"
      )(_.exists(_ == "\"[]\""))
    }

    "should return error message when expression is invalid" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 3, "1 ++ 2") {
        result =>
          result.left.exists { msg =>
            msg.format.contains("value ++ is not a member of Int")
          }
      }
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
      assertEvaluationInMainClass(source, "EvaluateTest", 4, "n")(
        _.exists(_.toInt == 1)
      )
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
      assertEvaluationInMainClass(source, "EvaluateTest", 4, "m1()")(
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
      assertEvaluationInMainClass(source, "EvaluateTest", 4, "m2()")(
        _.exists(_.toInt == 10)
      )
    }

    "should evaluate expression with breakpoint on an assignment" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = "Hello, World!"
          |    println(a)
          |  }
          |}
          |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 3, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "should evaluate expression with breakpoint on method definition" - {
      val source =
        """class Foo {
          |  def bar(): String = "foobar"
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 2, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "should evaluate expression definition" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 3, "val x = 123")(
        _.exists(_ == "<void value>")
      )
    }

    "should evaluate multi-line expression" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = 1
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        4,
        """val b = 2
          |val c = 3
          |a + b + c
          |""".stripMargin
      )(
        _.exists(_.toInt == 6)
      )
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        4,
        """{
          |val b = 2
          |val c = 3
          |a + b + c
          |}""".stripMargin
      )(
        _.exists(_.toInt == 6)
      )
    }

    "should evaluate expression with private method call" - {
      val source =
        """class Foo {
          |  val foo = this
          |
          |  def bar(): String = {
          |    p("a") // breakpoint
          |  }
          |
          |  def getFoo(): Foo = {
          |    if (true) foo
          |    else null
          |  }
          |
          |  private def p(a: String) = a
          |}
          |
          |object A {
          |  def getFoo() = new Foo()
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 5, "this.p(\"foo\")")(
        _.exists(_ == "\"foo\"")
      )
      assertEvaluationInMainClass(source, "EvaluateTest", 5, "foo.p(\"foo\")")(
        _.exists(_ == "\"foo\"")
      )
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        5,
        "getFoo().p(\"foo\")"
      )(
        _.exists(_ == "\"foo\"")
      )
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        5,
        "getFoo().getFoo().p(\"foo\")"
      )(
        _.exists(_ == "\"foo\"")
      )
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        5,
        "A.getFoo().p(\"foo\")"
      )(
        _.exists(_ == "\"foo\"")
      )
    }

    "should evaluate in default arguments" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    foo(3)()
           |  }
           |  def foo(x: Int)(
           |    y: Int = x + 1
           |  ): Unit = {
           |    println("foo")
           |  }
           |}
           |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 6, "x + 1")(
        _.exists(_.toInt == 4)
      )
    }

    "should evaluate nested method" - {
      val source =
        """class Foo {
          |  private val hello = "Hello"
          |  override def toString() = "foo"
          |  def bar() = {
          |    val sign = "!"
          |    def msg(name: String): String = {
          |      s"$hello, $name$sign"
          |    }
          |    def msg1(name: Int): String = {
          |      s"$hello, $name$sign"
          |    }
          |    def msg2(name: Foo): String = {
          |      s"$hello, $name$sign"
          |    }
          |    println(msg("World"))
          |  }
          |}
          |
          |object EvaluateTest {
          |  private val hello = "Hello"
          |  def main(args: Array[String]): Unit = {
          |    val sign = "!"
          |    def msg(name: String): String = {
          |      s"$hello, $name$sign"
          |    }
          |    def msg1(name: Int): String = {
          |      s"$hello, $name$sign"
          |    }
          |    def msg2(name: Foo): String = {
          |      s"$hello, $name$sign"
          |    }
          |    println(msg("World"))
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertEvaluationsInMainClass(
        source,
        "EvaluateTest",
        ExpressionEvaluation(
          15,
          """ msg("Alice") """,
          _.exists(_ == "\"Hello, Alice!\"")
        ),
        ExpressionEvaluation(
          15,
          """ msg1(1) """,
          _.exists(_ == "\"Hello, 1!\"")
        ),
        ExpressionEvaluation(
          15,
          """ msg2(new Foo) """,
          _.exists(_ == "\"Hello, foo!\"")
        ),
        ExpressionEvaluation(
          32,
          """ msg("Alice") """,
          _.exists(_ == "\"Hello, Alice!\"")
        ),
        ExpressionEvaluation(
          32,
          """ msg1(1) """,
          _.exists(_ == "\"Hello, 1!\"")
        ),
        ExpressionEvaluation(
          32,
          """ msg2(new Foo) """,
          _.exists(_ == "\"Hello, foo!\"")
        )
      )
    }

    "should evaluate tail-rec function" - {
      val source =
        """|object EvaluateTest {
           |  @scala.annotation.tailrec
           |  def f(x: Int): Int = {
           |    if (x <= 42) {
           |      x
           |    } else f(x/2)
           |  }
           |  def main(args: Array[String]): Unit = {
           |    val result = f(2)
           |  }
           |}
           |""".stripMargin
      assertEvaluationsInMainClass(
        source,
        "EvaluateTest",
        ExpressionEvaluation(5, "f(x)", _.exists(_.toInt == 2))
      )
    }

    "should keep working after success or failure" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    val result = 2
           |    println(result)
           |    println(result)
           |    println(result)
           |  }
           |}
           |""".stripMargin
      assertEvaluationsInMainClass(
        source,
        "EvaluateTest",
        ExpressionEvaluation(4, "result + 1", _.exists(_.toInt == 3)),
        ExpressionEvaluation(5, "resulterror", _.isLeft),
        ExpressionEvaluation(6, "result + 2", _.exists(_.toInt == 4))
      )
    }

    "should evaluate App block method" - {
      val source =
        """|object EvaluateTest extends App {
           |  val x = 1
           |  val y = {
           |    val msg = "Hello World!"
           |    println(msg)
           |    true
           |  }
           |}
           |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 6, "msg.toString()")(
        _.exists(_ == "\"Hello World!\"")
      )
    }

    "should evaluate at lambda start" - {
      val source =
        """|object EvaluateTest{
           |  def main(args: Array[String]): Unit = {
           |    val list = List(1, 2, 3)
           |    list.foreach { x =>
           |      println(x)
           |    }
           |  }
           |}
           |""".stripMargin
      assertEvaluationInMainClass(source, "EvaluateTest", 4, "1 + 1")(
        _.exists(_.toInt == 2)
      )
    }

    "should return exception as the result of evaluation" - {
      val source =
        """|object EvaluateTest{
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  def throwException(): Unit = {
           |    throw new Exception("error")
           |  }
           |}
           |""".stripMargin
      assertEvaluationInMainClass(
        source,
        "EvaluateTest",
        3,
        "throwException()"
      )(
        _.exists(_.contains("\"java.lang.Exception: error\""))
      )
    }

    "should evaluate in munit test (pending)" - {
      val source =
        """|class MySuite extends munit.FunSuite {
           |  def sum(list: List[Int]): Int = list.sum
           |
           |  test("sum of a few numbers") {
           |    assertEquals(sum(List(1,2,0)), 3)
           |  }
           |}""".stripMargin
      assertEvaluationsInTestSuite(
        source,
        "MySuite",
        ExpressionEvaluation(
          5,
          "1 + 1",
          _.exists(_ == "\"values are not the same\""),
          stoppageNo = 0
        ),
        // evaluating twice because the program stops twice at the same breakpoint...
        ExpressionEvaluation(
          5,
          "1 + 1",
          _.exists(_ == "\"values are not the same\""),
          stoppageNo = 1
        )
      )
    }

    "should evaluate lambdas" - {
      val source =
        """class Foo {
          |  val a = 1
          |  private val b = 2
          |  def bar() = {
          |    val c = 3
          |    println(s"a + b = ${a + b}")
          |  }
          |}
          |
          |object EvaluateTest {
          |  val a = 1
          |  private val b = 2
          |  def main(args: Array[String]): Unit = {
          |    val c = 3
          |    println(a + b + c)
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertEvaluationsInMainClass(
        source,
        "EvaluateTest",
        ExpressionEvaluation(
          6,
          "List(1, 2, 3).map(_ * a * b * c).sum",
          _.exists(_.contains("\"36\""))
        ),
        ExpressionEvaluation(
          15,
          "List(1, 2, 3).map(_ * a * b * c).sum",
          _.exists(_.contains("\"36\""))
        )
      )
    }
  }

  case class ExpressionEvaluation(
      line: Int,
      expression: String,
      assertion: Either[Message, String] => Boolean,
      stoppageNo: Int = 0
  )

  private def assertEvaluationInMainClass(
      source: String,
      mainClass: String,
      line: Int,
      expression: String
  )(assertion: Either[Message, String] => Boolean): Unit = {
    assertEvaluationsInMainClass(
      source,
      mainClass,
      ExpressionEvaluation(line, expression, assertion)
    )
  }

  private def assertEvaluationsInMainClass(
      source: String,
      mainClass: String,
      evaluationSteps: ExpressionEvaluation*
  ): Unit = {
    val runner =
      MainDebuggeeRunner.mainClassRunner(source, mainClass, scalaVersion)
    assertEvaluations(runner, evaluationSteps)
  }

  private def assertEvaluationsInTestSuite(
      source: String,
      testSuite: String,
      evaluationSteps: ExpressionEvaluation*
  ): Unit = {
    val runner =
      MainDebuggeeRunner.munitTestSuite(source, testSuite, scalaVersion)
    assertEvaluations(runner, evaluationSteps)
  }

  private def assertEvaluations(
      runner: MainDebuggeeRunner,
      evaluationSteps: Seq[ExpressionEvaluation]
  ): Unit = {
    val server = DebugServer(runner, NoopLogger)
    val client = TestDebugClient.connect(server.uri, 20.seconds)
    try {
      server.connect()
      client.initialize()
      client.launch()

      val lines = evaluationSteps.map(_.line).distinct.toArray
      val breakpoints = client.setBreakpoints(runner.source, lines)
      assert(breakpoints.length == lines.length)
      assert(breakpoints.forall(_.verified))
      client.configurationDone()

      evaluationSteps.groupBy(expr => (expr.line, expr.stoppageNo)).foreach {
        case ((line, _), expressionCases) =>
          val stopped = client.stopped()
          val threadId = stopped.threadId
          assert(stopped.reason == "breakpoint")

          val stackTrace = client.stackTrace(threadId)
          val topFrame = stackTrace.stackFrames.head
          expressionCases.foreach { expressionCase =>
            val result = client.evaluate(expressionCase.expression, topFrame.id)
            assert(expressionCase.assertion(result))
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
