package ch.epfl.scala.debugadapter

import utest._

object Scala212EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`2.12`)
object Scala213EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`2.13`)
object Scala30EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`3.0`)
object Scala31EvaluationTests extends ScalaEvaluationTests(ScalaVersion.`3.1`)

abstract class ScalaEvaluationTests(scalaVersion: ScalaVersion)
    extends ScalaEvaluationSuite(scalaVersion) {

  def tests: Tests = Tests {
    "evaluate local variables" - {
      val source =
        """|package example
           |object App {
           |  def main(args: Array[String]): Unit = {
           |    val str = "hello"
           |    val x1 = 1
           |    println("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.App")(
        Breakpoint(6)(
          ExpressionEvaluation.success("x1 + 2", 3),
          ExpressionEvaluation.success("str.reverse", "olleh")
        )
      )
    }

    "evaluate public and private fields in object" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  val a1 = "a1"
           |  private val a2 = "a2"
           |  private[this] val a3 = "a3"
           |  private[example] val a4 = "a4"
           |
           |  override def toString: String =
           |    a2 + a3
           |
           |  object B {
           |    val b1 = "b1"
           |    private val b2 = "b2"
           |    private[A] val b3 = "b3"
           |    private[example] val b4 = "b4"
           |  }
           |
           |  private object C
           |  private[this] object D
           |  private[example] object E
           |}
           |
           |object F {
           |  val f1 = "f1"
           |  private[example] val f2 = "f2"
           |
           |  object G
           |  private[example] object H
           |}
           |""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(5)(
          ExpressionEvaluation.success("a1", "a1"),
          ExpressionEvaluation.success("this.a1", "a1"),
          ExpressionEvaluation.success("A.this.a1", "a1"),
          ExpressionEvaluation.success("a2", "a2"),
          ExpressionEvaluation.successOrIgnore("a3", "a3", ignore = isScala2),
          ExpressionEvaluation.success("a4", "a4"),
          ExpressionEvaluation.success("B.b1", "b1"),
          ExpressionEvaluation.success("this.B.b1", "b1"),
          ExpressionEvaluation.success("A.B.b1", "b1"),
          ExpressionEvaluation.success("A.this.B.b1", "b1"),
          ExpressionEvaluation.failed("B.b2")(_ => true),
          ExpressionEvaluation.success("B.b3", "b3"),
          ExpressionEvaluation.success("A.B.b3", "b3"),
          ExpressionEvaluation.success("B.b4", "b4"),
          ExpressionEvaluation.success("C")(_.startsWith("A$C$@")),
          ExpressionEvaluation.success("D")(_.startsWith("A$D$@")),
          ExpressionEvaluation.success("F.f1", "f1"),
          ExpressionEvaluation.success("F.f2", "f2"),
          ExpressionEvaluation.success("F.G")(_.startsWith("F$G$@")),
          ExpressionEvaluation.success("F.H")(_.startsWith("F$H$@"))
        )
      )
    }

    "evaluate public and private methods in static object" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |
           |  def a1(str: String) = s"a1: $str"
           |  private def a2(str: String) = s"a2: $str"
           |  
           |  private object B {
           |    def b1(str: String) = s"b1: $str"
           |    private[A] def b2(str: String) = s"b2: $str"
           |  }
           |}
           |
           |object C {
           |  def c1(str: String) = s"c1: $str"
           |  private def c2(str: String) = s"c2: $str"
           |}
        """.stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(5)(
          ExpressionEvaluation.success("a1(\"foo\")", "a1: foo"),
          ExpressionEvaluation.success("a2(\"foo\")", "a2: foo"),
          ExpressionEvaluation.success("B.b1(\"foo\")", "b1: foo"),
          ExpressionEvaluation.success("B.b2(\"foo\")", "b2: foo"),
          ExpressionEvaluation.success("C.c1(\"foo\")", "c1: foo"),
          ExpressionEvaluation.failed("C.c2(\"foo\")")(_ => true)
        )
      )
    }

    "evaluate in private static object" - {
      val source =
        """|package example
           |
           |object A {
           |  private val a1 = "a1"
           |  private def a2(str: String): String = {
           |    s"a2: $str"
           |  }
           |  override def toString(): String = a1
           |  
           |  private object B {
           |    val b1 = "b1"
           |    def b2(str: String): String = {
           |      s"b2: $str"
           |    }
           |  }
           |
           |  def main(args: Array[String]): Unit = {
           |    println(B.b2("foo"))
           |  }
           |}
           |
           |object C {
           |  def c1(str: String) = s"c1: $str"
           |  private def c2(str: String) = s"c2: $str"
           |}
        """.stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(13)(
          ExpressionEvaluation.success("b1", "b1"),
          ExpressionEvaluation.success("b2(\"foo\")", "b2: foo"),
          ExpressionEvaluation.successOrIgnore("a1", "a1", ignore = isScala2),
          ExpressionEvaluation.successOrIgnore(
            "a2(\"foo\")",
            "a2: foo",
            ignore = isScala2
          )
        )
      )
    }

    "evaluate public and private fields in class" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A("a")
           |    println(a)
           |  }
           |}
           |
           |class A(name: String) {
           |  val a1 = s"$name.a1"
           |  private val a2 = s"$name.a2"
           |  
           |  object B {
           |    val  b1 = s"$name.B.b1"
           |  }
           |
           |  private object C  {
           |    val c1 = s"$name.C.c1"
           |  }
           |
           |  override def toString: String = {
           |    name + a2
           |  }
           |}
           |""".stripMargin
      println("TODO fix Scala 2")
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          ExpressionEvaluation.success("a.a1", "a.a1"),
          ExpressionEvaluation.success("a.B.b1", "a.B.b1"),
          ExpressionEvaluation.success("new A(\"aa\").a1", "aa.a1"),
          ExpressionEvaluation.success("new A(\"aa\").B.b1", "aa.B.b1")
        ),
        Breakpoint(23)(
          ExpressionEvaluation.success("name", "a"),
          ExpressionEvaluation.success("this.name", "a"),
          ExpressionEvaluation.success("a1", "a.a1"),
          ExpressionEvaluation.success("a2", "a.a2"),
          ExpressionEvaluation
            .successOrIgnore("new A(\"aa\").a2", "aa.a2", isScala2),
          ExpressionEvaluation.success("B.b1", "a.B.b1"),
          ExpressionEvaluation.success("this.B.b1", "a.B.b1"),
          ExpressionEvaluation.success("C.c1", "a.C.c1"),
          ExpressionEvaluation.success("new A(\"aa\").C.c1", "aa.C.c1")
        )
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

      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(5)(ExpressionEvaluation.success("m2()", 1))
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
      assertInMainClass(source, "debug.EvaluateTest")(
        Breakpoint(4)(ExpressionEvaluation.success("1 + 2", 3))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(ExpressionEvaluation.success("n", 1))
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
        assertInMainClass(source, "EvaluateTest")(
          Breakpoint(4)(),
          Breakpoint(4)(ExpressionEvaluation.success("n", 1))
        )
      } else {
        assertInMainClass(source, "EvaluateTest")(
          Breakpoint(4)(ExpressionEvaluation.success("n", 1))
        )
      }
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(ExpressionEvaluation.success("m1()", 9))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(ExpressionEvaluation.success("m2()", 10))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(3)(ExpressionEvaluation.success("1 + 2", 3))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(3)(ExpressionEvaluation.success("a + 2", 3))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(2)(ExpressionEvaluation.success("1 + 2", 3))
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
          ExpressionEvaluation.success("p(\"foo\")", "foo"),
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(ExpressionEvaluation.success("x + 1", 4))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(4)(ExpressionEvaluation.success("1 + 1", 2))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(
          ExpressionEvaluation.success("List(1, 2, 3).map(_ * 2).sum", 12)
        )
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
        ExpressionEvaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36)
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(evaluation),
        Breakpoint(15)(evaluation)
      )
    }

    "evaluate call to anonymous function" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val f = (s: String) => s.size
           |    println(f("Hello world"))
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          ExpressionEvaluation.success("f(\"foo\")")(_.contains("3"))
        )
      )
    }

    "evaluate call to method of generic class" - {
      val source =
        """|package example
           |
           |class Writer[T](f: T => Unit) {
           |  def write(value: T): Unit = {
           |    f(value)
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val writer = new Writer[String](println(_))
           |    writer.write("Hello, World!")
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          ExpressionEvaluation.success("write(value)", ()),
          // Should it work without casting?
          // In contravariant case, we could find what's the expected type
          // In the covariant case, it is not possible to know what the precise return type is at runtime
          ExpressionEvaluation.success("write(\"Hello\".asInstanceOf[T])", ())
        )
      )
    }

    "evaluate call to anonymous polymorphic function" - {
      val source =
        """|package example
           |
           |object Foo {
           |  def foo[A](f: String => A): A = {
           |    f("foo")
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    Foo.foo(_.reverse)
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          ExpressionEvaluation.success("f(\"foo\")", "oof")
        )
      )
    }
  }
}
