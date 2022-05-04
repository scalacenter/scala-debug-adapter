package ch.epfl.scala.debugadapter

import utest._

object MoreScala30EvaluationTests
    extends MoreScala3EvaluationTests(ScalaVersion.`3.0`)
object MoreScala31EvaluationTests
    extends MoreScala3EvaluationTests(ScalaVersion.`3.1`)

abstract class MoreScala3EvaluationTests(scalaVersion: ScalaVersion)
    extends ScalaEvaluationSuite(scalaVersion) {

  override def tests: Tests = Tests {
    "evaluate shadowed variable properly" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    val foo = "foo"
           |    {
           |      val foo = "bar"
           |      println(foo)
           |    }
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(ExpressionEvaluation.success("foo", "bar"))
      )
    }

    "evaluate variable shadowed in other scope properly" - {
      val source =
        """|object EvaluateTest {
           |  def main(args: Array[String]): Unit = {
           |    val foo = "foo"
           |    {
           |      val foo = "bar"
           |      println(foo)
           |    }
           |    println(foo)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(8)(ExpressionEvaluation.success("foo", "foo"))
      )
    }

    "brace-less syntax: evaluate expression with object's public fields" - {
      val source =
        """object A:
          |  val x1 = "x1"
          |
          |object EvaluateTest:
          |  val x1 = 1.1
          |  val x2 = 2.2
          |
          |  def main(args: Array[String]): Unit =
          |    println("Hello, World!")
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(9)(
          ExpressionEvaluation.success("x1 + x2")(_.startsWith("3.3")),
          ExpressionEvaluation.success("A.x1", "x1")
        )
      )
    }

    "brace-less syntax: evaluate expression with class's private fields" - {
      val source =
        """class A:
          |  private val x1 = "x1"
          |
          |  def m1(): Unit =
          |    println(x1)
          |
          |object EvaluateTest:
          |  def main(args: Array[String]): Unit =
          |    new A().m1()
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 5, "x1")(
        _.exists(_ == "\"x1\"")
      )
    }

    "brace-less syntax: evaluate expression with outer class's public fields" - {
      val source =
        """class A:
          |  val x1 = "x1"
          |  class B:
          |    val x2 = "x2"
          |    def m1(): Unit =
          |      val x3 = "x3"
          |      println(x1 + x2 + x3)
          |
          |object EvaluateTest:
          |  def main(args: Array[String]): Unit =
          |    val a = new A()
          |    val b = new a.B()
          |    b.m1()
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(7)(), // Stops once in the constructor of B
        Breakpoint(7)(ExpressionEvaluation.success("x1 + x2 + x3", "x1x2x3"))
      )
    }

    "brace-less syntax: evaluate expression in package" - {
      val source =
        """package debug:
          |  object EvaluateTest:
          |    def main(args: Array[String]): Unit =
          |      println("Hello, World!")
          |""".stripMargin
      assertInMainClass(source, "debug.EvaluateTest", 4, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "brace-less syntax: evaluate expression with breakpoint on method definition" - {
      val source =
        """class Foo:
          |  def bar(): String = "foobar"
          |
          |object EvaluateTest:
          |  def main(args: Array[String]): Unit =
          |    new Foo().bar()
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 2, "1 + 2")(
        _.exists(_.toInt == 3)
      )
    }

    "@main: evaluate expression definition" - {
      val source =
        """|@main def foo(): Unit =
           |  println("Hello, World!")
           |""".stripMargin
      assertInMainClass(source, "foo", 2, "val x = 123")(
        _.exists(result => result == "<void value>" || result.contains("()"))
      )
    }

    "brace-less syntax: evaluate expression with private method call" - {
      val source =
        """class Foo:
          |  val foo = this
          |
          |  def bar(): String =
          |    p("a")
          |
          |  def getFoo(): Foo =
          |    if (true) foo
          |    else null
          |
          |  private def p(a: String) = a
          |
          |object A:
          |  def getFoo() = new Foo()
          |
          |object EvaluateTest:
          |  def main(args: Array[String]): Unit =
          |    new Foo().bar()
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

    "brace-less syntax: evaluate in default arguments" - {
      val source =
        """|object EvaluateTest:
           |  def main(args: Array[String]): Unit =
           |    foo(3)()
           |  
           |  def foo(x: Int)(
           |    y: Int = x + 1
           |  ): Unit =
           |    println("foo")
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest", 6, "x + 1")(
        _.exists(_.toInt == 4)
      )
    }

    "brace-less syntax: evaluate nested method" - {
      val source =
        """class Foo:
          |  private val hello = "Hello"
          |  override def toString() = "foo"
          |  def bar() =
          |    val sign = "!"
          |    def msg(name: String): String =
          |      s"$hello, $name$sign"
          |    def msg1(name: Int): String =
          |      s"$hello, $name$sign"
          |    def msg2(name: Foo): String =
          |      s"$hello, $name$sign"
          |    println(msg("World"))
          |
          |object EvaluateTest:
          |  def main(args: Array[String]): Unit =
          |    new Foo().bar()
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(12)(
          ExpressionEvaluation.success("msg(\"Alice\")", "Hello, Alice!"),
          ExpressionEvaluation.success("msg1(1)", "Hello, 1!"),
          ExpressionEvaluation.success("msg2(new Foo)", "Hello, foo!")
        )
      )
    }

    "brace-less syntax: evaluate tail-rec function" - {
      val source =
        """|object EvaluateTest:
           |  @scala.annotation.tailrec
           |  def f(x: Int): Int =
           |    if (x <= 42) then
           |      x
           |    else f(x/2)
           |
           |  def main(args: Array[String]): Unit =
           |    val result = f(2)
           |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(5)(ExpressionEvaluation.success("f(x)", 2))
      )
    }
  }
}
