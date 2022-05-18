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
          Evaluation.success("x1 + 2", 3),
          Evaluation.success("str.reverse", "olleh")
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
          Evaluation.success("a1", "a1"),
          Evaluation.success("this.a1", "a1"),
          Evaluation.success("A.this.a1", "a1"),
          Evaluation.success("a2", "a2"),
          Evaluation.successOrIgnore("a3", "a3", ignore = isScala2),
          Evaluation.success("a4", "a4"),
          Evaluation.success("B.b1", "b1"),
          Evaluation.success("this.B.b1", "b1"),
          Evaluation.success("A.B.b1", "b1"),
          Evaluation.success("A.this.B.b1", "b1"),
          Evaluation.failed("B.b2")(_ => true),
          Evaluation.success("B.b3", "b3"),
          Evaluation.success("A.B.b3", "b3"),
          Evaluation.success("B.b4", "b4"),
          Evaluation.success("C")(_.startsWith("A$C$@")),
          Evaluation.success("D")(_.startsWith("A$D$@")),
          Evaluation.success("F.f1", "f1"),
          Evaluation.success("F.f2", "f2"),
          Evaluation.success("F.G")(_.startsWith("F$G$@")),
          Evaluation.success("F.H")(_.startsWith("F$H$@"))
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
          Evaluation.success("a1(\"foo\")", "a1: foo"),
          Evaluation.success("a2(\"foo\")", "a2: foo"),
          Evaluation.success("B.b1(\"foo\")", "b1: foo"),
          Evaluation.success("B.b2(\"foo\")", "b2: foo"),
          Evaluation.success("C.c1(\"foo\")", "c1: foo"),
          Evaluation.failed("C.c2(\"foo\")")(_ => true)
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
          Evaluation.success("b1", "b1"),
          Evaluation.success("b2(\"foo\")", "b2: foo"),
          Evaluation.successOrIgnore("a1", "a1", ignore = isScala2),
          Evaluation.successOrIgnore(
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          Evaluation.success("a.a1", "a.a1"),
          Evaluation.success("a.B.b1", "a.B.b1"),
          Evaluation.success("new A(\"aa\").a1", "aa.a1"),
          Evaluation.success("new A(\"aa\").B.b1", "aa.B.b1")
        ),
        Breakpoint(23)(
          Evaluation.success("name", "a"),
          Evaluation.success("this.name", "a"),
          Evaluation.success("a1", "a.a1"),
          Evaluation.success("a2", "a.a2"),
          Evaluation.successOrIgnore("new A(\"aa\").a2", "aa.a2", isScala2),
          Evaluation.success("B.b1", "a.B.b1"),
          Evaluation.success("this.B.b1", "a.B.b1"),
          Evaluation.success("C.c1", "a.C.c1"),
          Evaluation.success("new A(\"aa\").C.c1", "aa.C.c1")
        )
      )
    }

    "evaluate private overloaded method" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |  
           |  trait A
           |  class B extends A
           |  
           |  private def m(): String = "m"
           |  private def m(n: Int): String = s"m($n: Int)"
           |  private def m(b: Boolean): String = s"m($b: Boolean)"
           |  private def m(str: String): String = s"m($str: String)"
           |  private def m(a: A): String = s"m(a: A)"
           |  private def m(b: B): String = s"m(b: B)"
           |  private def m(xs: Array[Int]): String = s"m(xs: Array[Int])"
           |  private def m(xs: Array[A]): String = s"m(xs: Array[A])"
           |  private def m(xs: Array[Array[Int]]): String = s"m(xs: Array[Array[Int]])"
           |
           |  private def m1(xs: Seq[Int]): String = xs.toString
           |  private def m1(xs: Seq[Boolean]): Int = xs.count(identity)
           |}
           |
           |
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(
          Evaluation.success("m()", "m"),
          Evaluation.success("m(5)", "m(5: Int)"),
          Evaluation.success("m(true)", "m(true: Boolean)"),
          Evaluation.success("m(\"foo\")", "m(foo: String)"),
          Evaluation.successOrIgnore("m(new B)", "m(b: B)", isScala2),
          Evaluation.successOrIgnore("m(new B: A)", "m(a: A)", isScala2),
          Evaluation
            .successOrIgnore("m(Array(1, 2))", "m(xs: Array[Int])", isScala2),
          Evaluation
            .successOrIgnore("m(Array[A](new B))", "m(xs: Array[A])", isScala2),
          Evaluation.successOrIgnore(
            "m(Array(Array(1), Array(2)))",
            "m(xs: Array[Array[Int]])",
            isScala2
          ),
          Evaluation
            .successOrIgnore("m1(Seq(1, 2, 3))", "List(1, 2, 3)", isScala2),
          Evaluation.successOrIgnore(
            "m1(Vector(1, 2, 3))",
            "Vector(1, 2, 3)",
            isScala2
          ),
          Evaluation.successOrIgnore("m1(Seq(true, false, true))", 2, isScala2)
        )
      )
    }

    "evaluate private inner class" - {
      val source =
        """|package example
           |
           |object A {
           |  def main(args: Array[String]): Unit = {
           |    val c = new C
           |    c.c1()
           |  }
           |
           |  private def a1(): B = new B
           |  private def a2(b: B): String = "a2"
           |
           |  private class B {
           |    val b1: String = "b1"
           |    def b2(): String = "b2"
           |  }
           |}
           |
           |class C {
           |  def c1(): Unit =
           |    println("Hello, World!")
           |  
           |  private def c2(): D = new D
           |  private def c3(d: D): String = "c3"
           |
           |  private class D {
           |    val d1: String = "d1"
           |    def d2(): String = "d2"
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.A")(
        Breakpoint(5)(
          Evaluation.success("a1()")(_.startsWith("A$B@")),
          Evaluation.success("(new B).b1", "b1"),
          Evaluation.success("(new A.B).b2()", "b2"),
          Evaluation.success("a2(new B)", "a2")
        ),
        Breakpoint(20)(
          Evaluation.success("c2()")(_.startsWith("C$D@")),
          Evaluation.success("(new D).d1", "d1"),
          Evaluation.success("(new this.D).d2()", "d2"),
          Evaluation.success("c3(new D)", "c3")
        )
      )
    }

    "evaluate shaded fields and values" - {
      val source =
        """|package example
           |
           |class A {
           |  val x1 = "ax1"
           |  val x2 = "ax2"
           |  class B {
           |    val x2 = "bx2"
           |    val x3 = "bx3"
           |    def m1(): Unit = {
           |      val x3 = "x3"
           |      println(x1 + x2 + x3)
           |    }
           |  }
           |}
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A()
           |    val b = new a.B()
           |    b.m1()
           |  }
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(11)(
          Evaluation.success("x1 + x2 + x3", "ax1bx2x3"),
          Evaluation.successOrIgnore(
            "x1 + A.this.x2 + this.x3",
            "ax1ax2bx3",
            isScala2
          )
        )
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
        Breakpoint(4)(Evaluation.success("1 + 2", 3))
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
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    List(1).foreach(n => {
           |      println(n)
           |    })
           |    List(1).foreach { n =>
           |      println(n)
           |    }
           |  }
           |
           |  def m1(): Int = 9
           |}
           |""".stripMargin
      val evaluations =
        Seq(
          Breakpoint(6)(
            Evaluation.success("n", 1),
            Evaluation.success("m1()", 9)
          )
        ) ++
          (if (isScala3) Some(Breakpoint(9)()) else None) :+
          Breakpoint(9)(
            Evaluation.success("n", 1),
            Evaluation.success("m1()", 9)
          )

      assertInMainClass(source, "example.Main")(evaluations: _*)
    }

    "evaluate expression with breakpoint on an assignment" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val foo = new Foo
           |    println(foo.toString)
           |  }
           |}
           |
           |class Foo {
           |  val a = 1
           |  val b = 2
           |}
           |""".stripMargin
      assertInMainClass(source, "example.Main")(
        Breakpoint(5)(Evaluation.success("1 + 2", 3)),
        Breakpoint(12)(Evaluation.success("a + 2", 3))
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
        Breakpoint(2)(Evaluation.success("1 + 2", 3))
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
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(3)(Evaluation.success("val x = 123", ()))
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
          Evaluation.success(
            """val b = 2
              |val c = 3
              |a + b + c
              |""".stripMargin,
            6
          )
        )
      )
    }

    "evaluate private method call in class" - {
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
          Evaluation.success("p(\"foo\")", "foo"),
          Evaluation.success("this.p(\"foo\")", "foo"),
          Evaluation.success("foo.p(\"foo\")", "foo"),
          Evaluation.success("getFoo().p(\"foo\")", "foo"),
          Evaluation.success("getFoo().getFoo().p(\"foo\")", "foo"),
          Evaluation.success("A.getFoo().p(\"foo\")", "foo")
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
        Breakpoint(6)(Evaluation.success("x + 1", 4))
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
          Evaluation.success("msg(\"Alice\")", "Hello, Alice!"),
          Evaluation.success("msg1(1)", "Hello, 1!"),
          Evaluation.success("msg2(new Foo)", "Hello, foo!")
        ),
        Breakpoint(32)(
          Evaluation.success("msg(\"Alice\")", "Hello, Alice!"),
          Evaluation.success("msg1(1)", "Hello, 1!"),
          Evaluation.success("msg2(new Foo)", "Hello, foo!")
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
        Breakpoint(5)(Evaluation.success("f(x)", 2))
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
        Breakpoint(4)(Evaluation.success("result + 1", 3)),
        Breakpoint(5)(Evaluation.failed("resulterror")(_ => true)),
        Breakpoint(6)(Evaluation.success("result + 2", 4))
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
        Breakpoint(4)(Evaluation.success("1 + 1", 2))
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
          Breakpoint(5)(Evaluation.success("1 + 1", 2))
        )
      } else {
        assertInTestSuite(source, "MySuite")(
          Breakpoint(5)(), // the program stops twice...
          Breakpoint(5)(Evaluation.success("1 + 1", 2))
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
          |    val c = 3
          |    println(a + b + c)
          |    new Foo().bar()
          |  }
          |}
          |""".stripMargin
      assertInMainClass(source, "EvaluateTest")(
        Breakpoint(6)(
          Evaluation.success("List(1, 2, 3).map(_ * 2).sum", 12),
          Evaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36)
        ),
        Breakpoint(15)(
          Evaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36)
        )
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
          Evaluation.success("f(\"foo\")")(_.contains("3"))
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
          Evaluation.success("write(value)", ()),
          // Should it work without casting?
          // In contravariant case, we could find what's the expected type
          // In the covariant case, it is not possible to know what the precise return type is at runtime
          Evaluation.success("write(\"Hello\".asInstanceOf[T])", ())
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
          Evaluation.success("f(\"foo\")", "oof")
        )
      )
    }
  }
}
