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
        Breakpoint(6)(Evaluation.success("foo", "bar"))
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
        Breakpoint(8)(Evaluation.success("foo", "foo"))
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
          Evaluation.success("x1 + x2")(_.contains("3.3")),
          Evaluation.success("A.x1", "x1")
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
        Breakpoint(7)(Evaluation.success("x1 + x2 + x3", "x1x2x3"))
      )
    }

    "brace-less syntax: evaluate expression in package" - {
      val source =
        """package debug:
          |  object EvaluateTest:
          |    def main(args: Array[String]): Unit =
          |      println("Hello, World!")
          |""".stripMargin
      assertInMainClass(source, "debug.EvaluateTest")(
        Breakpoint(4)(Evaluation.success("1 + 2", 3))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(2)(Evaluation.success("1 + 2", 3))
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
          Evaluation.success("this.p(\"foo\")", "foo"),
          Evaluation.success("foo.p(\"foo\")", "foo"),
          Evaluation.success("getFoo().p(\"foo\")", "foo"),
          Evaluation.success("getFoo().getFoo().p(\"foo\")", "foo"),
          Evaluation.success("A.getFoo().p(\"foo\")", "foo")
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(Evaluation.success("x + 1", 4))
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
          Evaluation.success("msg(\"Alice\")", "Hello, Alice!"),
          Evaluation.success("msg1(1)", "Hello, 1!"),
          Evaluation.success("msg2(new Foo)", "Hello, foo!")
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
        Breakpoint(5)(Evaluation.success("f(x)", 2))
      )
    }

    "evaluate inline def" - {
      val source =
        """|package example
           |
           |object Main:
           |  def main(args: Array[String]): Unit =
           |    val msg: String = "Hello, World!"
           |    println(foo(msg, 5))
           |
           |  private inline def foo(str: String, n: Int): String = str.take(n)
           |
           |  inline def bar(inline str: String): String = str.reverse
           |
           |""".stripMargin

      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          Evaluation.success("msg", "Hello, World!"),
          Evaluation.success("foo(msg, 5)", "Hello"),
          Evaluation.success("foo(msg + \"!!\", 4)", "Hell"),
          Evaluation.success("bar(foo(msg, 5))", "olleH")
        ),
        Breakpoint(6)()
      )
    }

    "evaluate macro def" - {
      val mainSource =
        """|package example
           |
           |import Macro.showType
           |
           |object Main:
           |  def main(args: Array[String]): Unit =
           |    val msg: String = "Hello, World!"
           |    println(showType(msg))
           |""".stripMargin

      val macroSource =
        """|package example
           |
           |import scala.quoted.*
           |
           |object Macro:
           |  inline def showType(inline expr: Any): String = ${ showTypeImpl('expr) }
           |
           |  private def showTypeImpl(expr: Expr[Any])(using Quotes): Expr[String] = 
           |    import quotes.reflect.*
           |    Expr(expr.asTerm.tpe.widen.show)
           |""".stripMargin

      assertInMainClass(
        Seq("Main.scala" -> mainSource, "Macro.scala" -> macroSource),
        "example.Main"
      )(
        Breakpoint(8)(
          Evaluation.success("msg", "Hello, World!"),
          Evaluation.success("showType(msg)", "scala.Predef.String"),
          Evaluation.success(
            """|type Foo = Int
               |showType(1: Foo)""".stripMargin,
            "Foo"
          ),
          Evaluation.success(
            """|trait Foo
               |class Bar extends Foo
               |val foo: Foo = new Bar
               |showType(foo)
               |""".stripMargin,
            "Foo"
          )
        )
      )
    }
  }
}
