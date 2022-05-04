package ch.epfl.scala.debugadapter

import utest._
import java.beans.Expression

object Scala212EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`2.12`)
object Scala213EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`2.13`)
object Scala30EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`3.0`)
object Scala31EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`3.1`)

abstract class ScalaEvaluationTests(scalaVersion: ScalaVersion)
    extends ScalaEvaluationSuite(scalaVersion) {

  def tests: Tests = Tests {
    "evaluate expression with primitives" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val helloWorld = "Hello, World!"
          |    println(helloWorld)
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 4, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "evaluate expression with primitive local variables" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val x1 = 1.1
          |    val x2 = 2.2
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 5, "x1 + x2")(
        _.exists(_.toDouble == 3.3)
      )
    }

    "evaluate expression with non-primitive local variables" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val x1 = "foo"
          |    val x2 = "bar"
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 5, "x1 + x2")(
        _.exists(_ == "\"foobar\"")
      )
    }

    "evaluate expression with object's public fields" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(10)(
          ExpressionEvaluation.success("x1 + x2")(_.startsWith("3.3")),
          ExpressionEvaluation.success("A.x1", "x1")
        )
      )
    }

    "evaluate expression with object's private fields" - {
      val source =
        """object EvaluateTest {
          |  private val x1 = 1.1
          |  private val x2 = 2.2
          |
          |  def main(args: Array[String]): Unit = {
          |    println(s"x1 + x2 = $x1 + $x2")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 6, "x1 + x2")(
        _.exists(_.toDouble == 3.3)
      )
    }

    "evaluate expression with class's public fields" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(11)(ExpressionEvaluation.success("new A().x1", "x1")),
        Breakpoint(5)(ExpressionEvaluation.success("x1", "x1"))
      )
    }

    "evaluate expression with class's private fields" - {
      val source =
        """class A {
          |  private val x1 = "x1"
          |
          |  def m1(): Unit = {
          |    println(x1)
          |  }
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new A().m1()
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 5, "x1")(
        _.exists(_ == "\"x1\"")
      )
    }

    "evaluate expression with inner class's public fields" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(15)(ExpressionEvaluation.success("b.x1", "x1")),
        Breakpoint(6)(ExpressionEvaluation.success("x1", "x1"))
      )
    }

    "evaluate expression with inner class's private fields" - {
      val source =
        """class A {
          |  class B {
          |    private val x1 = "x1"
          |
          |    def m1(): Unit = {
          |      println(x1)
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
      assertInMainClass(source, "EvaluateTest", 6, "x1")(
        _.exists(_ == "\"x1\"")
      )
    }

    "evaluate expression with outer class's public fields" - {
      val source =
        """class A {
          |  val x1 = "x1"
          |  class B {
          |    val x2 = "x2"
          |    def m1(): Unit = {
          |      val x3 = "x3"
          |      println(x1 + x2 + x3)
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
      assertInMainClass(source, "EvaluateTest", 7, "x1 + x2 + x3")(
        _.exists(_ == "\"x1x2x3\"")
      )
    }

    "evaluate expression with a public method call" - {
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

      assertInMainClass(source, "EvaluateTest", 5, "m2()")(
        _.exists(_.toInt == 1)
      )
    }

    "evaluate expression with inner class's overridden fields" - {
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
      assertInMainClass(source, "EvaluateTest", 7, "x1")(
        _.exists(_ == "\"x1x1\"")
      )
    }

    "evaluate expression in package" - {
      val source =
        """package debug {
          |object EvaluateTest {
          |    def main(args: Array[String]): Unit = {
          |      println("Hello, World!")
          |    }
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "debug.EvaluateTest", 4, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "evaluate expression with Java util code" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(
        source,
        "EvaluateTest",
        3,
        "new java.util.ArrayList[String]().toString"
      )(_.exists(_ == "\"[]\""))
    }

    "return error message when expression is invalid" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 3, "1 ++ 2") { result =>
        result.left.exists { msg =>
          msg.format.contains("value ++ is not a member of Int")
        }
      }
    }

    "evaluate expression inside of a lambda" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    List(1).foreach(n => {
          |      println(n)
          |    })
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 4, "n")(
        _.exists(_.toInt == 1)
      )
    }

    "evaluate expression inside of a lambda - 2" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    List(1).foreach { n =>
          |      println(n)
          |    }
          |  }
          |}
          |""".stripMargin
      if (isScala3) {
        println("TODO fix") // TODO fix
      } else
        assertInMainClass(source, "EvaluateTest")(
          Breakpoint(4)(ExpressionEvaluation.success("n", 1))
        )
    }

    "evaluate expression a object's method call inside of a lambda" - {
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
      assertInMainClass(source, "EvaluateTest", 4, "m1()")(
        _.exists(_.toInt == 9)
      )
    }

    "evaluate expression a class's method call inside of a lambda" - {
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
      assertInMainClass(source, "EvaluateTest", 4, "m2()")(
        _.exists(_.toInt == 10)
      )
    }

    "evaluate expression with breakpoint on an assignment" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = "Hello, World!"
          |    println(a)
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 3, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "evaluate expression with breakpoint on a field's assignment" - {
      val source =
        """class Foo {
          |  val a = 1
          |  val b = 2
          |  def bar() = a + b
          |}
          |
          |object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    new Foo()
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 3, "a + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "evaluate expression with breakpoint on method definition" - {
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
      assertInMainClass(source, "EvaluateTest", 2, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "evaluate expression definition" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 3, "val x = 123")(
        _.exists(result => result == "<void value>" || result.contains("()"))
      )
    }

    "evaluate multi-line expression" - {
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a = 1
          |    println("Hello, World!")
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(
          ExpressionEvaluation.success(
            """val b = 2
              |val c = 3
              |a + b + c
              |""".stripMargin,
            6
          )
        )
      )
    }

    "evaluate expression with private method call" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(5)(
          ExpressionEvaluation.success("this.p(\"foo\")", "foo"),
          ExpressionEvaluation.success("foo.p(\"foo\")", "foo"),
          ExpressionEvaluation.success("getFoo().p(\"foo\")", "foo"),
          ExpressionEvaluation.success("getFoo().getFoo().p(\"foo\")", "foo"),
          ExpressionEvaluation.success("A.getFoo().p(\"foo\")", "foo")
        )
      )
    }

    "evaluate in default arguments" - {
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
      assertInMainClass(source, "EvaluateTest", 6, "x + 1")(
        _.exists(_.toInt == 4)
      )
    }

    "evaluate nested method" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(15)(
          ExpressionEvaluation.success("msg(\"Alice\")", "Hello, Alice!"),
          ExpressionEvaluation.success("msg1(1)", "Hello, 1!"),
          ExpressionEvaluation.success("msg2(new Foo)", "Hello, foo!")
        ),
        Breakpoint(32)(
          ExpressionEvaluation.success("msg(\"Alice\")", "Hello, Alice!"),
          ExpressionEvaluation.success("msg1(1)", "Hello, 1!"),
          ExpressionEvaluation.success("msg2(new Foo)", "Hello, foo!")
        )
      )
    }

    "evaluate tail-rec function" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(5)(ExpressionEvaluation.success("f(x)", 2))
      )
    }

    "keep working after success or failure" - {
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(ExpressionEvaluation.success("result + 1", 3)),
        Breakpoint(5)(ExpressionEvaluation.failed("resulterror")(_ => true)),
        Breakpoint(6)(ExpressionEvaluation.success("result + 2", 4))
      )
    }

    "evaluate App block method" - {
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
      assertInMainClass(source, "EvaluateTest", 6, "msg.toString()")(
        _.exists(_ == "\"Hello World!\"")
      )
    }

    "evaluate at lambda start" - {
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
      assertInMainClass(source, "EvaluateTest", 4, "1 + 1")(
        _.exists(_.toInt == 2)
      )
    }

    "return exception as the result of evaluation" - {
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
      assertInMainClass(
        source,
        "EvaluateTest",
        3,
        "throwException()"
      )(
        _.exists(_.contains("\"java.lang.Exception: error\""))
      )
    }

    "evaluate in munit test" - {
      val source =
        """|class MySuite extends munit.FunSuite {
           |  def sum(list: List[Int]): Int = list.sum
           |
           |  test("sum of a few numbers") {
           |    assertEquals(sum(List(1,2,0)), 3)
           |  }
           |}""".stripMargin

      if (isScala31) {
        assertInTestSuite(source, "MySuite")(
          Breakpoint(5)(ExpressionEvaluation.success("1 + 1", 2))
        )
      } else {
        assertInTestSuite(source, "MySuite")(
          Breakpoint(5)(), // the program stops twice...
          Breakpoint(5)(ExpressionEvaluation.success("1 + 1", 2))
        )
      }
    }

    "evaluate lambdas" - {
      val source =
        """class Foo {
          |  val a = 1
          |  private val b = 2
          |  def bar() = {
          |    val c = 3
          |    println(s"a + b + c = ${a + b + c}")
          |  }
          |}
          |
          |object EvaluateTest {
          |  val a = 1
          |  private val b = 2
          |  def main(args: Array[String]): Unit = {
          |    val c = 1
          |    println(a + b + c)
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      val evaluation =
        if (isScala3)
          ExpressionEvaluation.success("List(1, 2, 3).map(_ * 2).sum", 12)
        else
          ExpressionEvaluation.success("List(1, 2, 3).map(_ * 2).sum")(
            _.contains("12")
          )
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(evaluation)
      )
    }

    "evaluate closures" - {
      val source =
        """class Foo {
          |  val a = 1
          |  private val b = 2
          |  def bar() = {
          |    val c = 3
          |    println(s"a + b + c = ${a + b + c}")
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
      val evaluation =
        if (isScala3)
          ExpressionEvaluation.success(
            "List(1, 2, 3).map(_ * a * b * c).sum",
            36
          )
        else
          ExpressionEvaluation.success("List(1, 2, 3).map(_ * a * b * c).sum")(
            _.contains("36")
          )
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(evaluation),
        Breakpoint(15)(evaluation)
      )
    }
  }
}
