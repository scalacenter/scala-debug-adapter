package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class Scala212EvaluationTests extends Scala2EvaluationTests(ScalaVersion.`2.12`)
class Scala213EvaluationTests extends Scala2EvaluationTests(ScalaVersion.`2.13`) {
  if (ScalaVersion.`3.1+`.isRelease) {
    test("should use tasty-reader") {
      val scala2Source =
        """|package example
           |
           |trait Msg
           |
           |object Sender {
           |  def send(msg: Msg): Unit = {
           |    println(msg)
           |  }
           |}
           |""".stripMargin

      val scala2Debugee = TestingDebuggee.mainClass(scala2Source, "example.Sender", scalaVersion)
      val scala3Source =
        """|package example
           |
           |case class Scala3Msg(msg: String) extends Msg:
           |  override def toString: String = msg
           |
           |object Main:
           |  def main(args: Array[String]): Unit =
           |    Sender.send(Scala3Msg("Hello"))
           |""".stripMargin

      implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(
        scala3Source,
        "example.Main",
        ScalaVersion.`3.1+`,
        Seq.empty,
        Seq(scala2Debugee.mainModule)
      )
      check(
        Breakpoint(scala2Debugee.sourceFiles.head, 7),
        Evaluation.success("msg.asInstanceOf[Scala3Msg].msg", "Hello")
      )
    }
  }
}
class Scala30EvaluationTests extends Scala3EvaluationTests(ScalaVersion.`3.0`)
class Scala31PlusEvaluationTests extends Scala3EvaluationTests(ScalaVersion.`3.1+`)

abstract class ScalaEvaluationTests(scalaVersion: ScalaVersion) extends DebugTestSuite {
  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.ScalaEvaluationOnly)

  test("report source and position in error, and no colors") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.failedOrIgnore("\"foo\" + bar", isScala2) { error =>
        assert(error.contains("<expression>:1:8"))
        assert(error.contains("\"foo\" + bar"))
      },
      Evaluation.failedOrIgnore("bar", isScala2)(error => assert(!error.contains('\u001b')))
    )
  }

  test("evaluate primitive values") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(4),
      DebugStepAssert.inParallel(
        Evaluation.success("true", true),
        Evaluation.success("0: Byte", 0: Byte),
        Evaluation.success("'a'", 'a'),
        Evaluation.success("1.0D", 1.0d),
        Evaluation.success("0.42F", 0.42f),
        Evaluation.success("42", 42),
        Evaluation.success("42L", 42L),
        Evaluation.success("42: Short", 42: Short)
      )
    )
  }

  test("evaluate local variables") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val str = "hello"
         |    val x1 = 1
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      Evaluation.success("x1 + 2", 3),
      Evaluation.success("str.reverse", "olleh")
    )
  }

  test("evaluate public and private fields in object") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(5),
      DebugStepAssert.inParallel(
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
        Evaluation.failed("B.b2"),
        Evaluation.success("B.b3", "b3"),
        Evaluation.success("A.B.b3", "b3"),
        Evaluation.success("B.b4", "b4"),
        Evaluation.success("C")(result => assert(result.startsWith("A$C$@"))),
        Evaluation.success("D")(result => assert(result.startsWith("A$D$@"))),
        Evaluation.success("F.f1", "f1"),
        Evaluation.success("F.f2", "f2"),
        Evaluation.success("F.G")(result => assert(result.startsWith("F$G$@"))),
        Evaluation.success("F.H")(result => assert(result.startsWith("F$H$@")))
      )
    )
  }

  test("evaluate public and private methods in static object") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(5),
      DebugStepAssert.inParallel(
        Evaluation.success("a1(\"foo\")", "a1: foo"),
        Evaluation.success("a2(\"foo\")", "a2: foo"),
        Evaluation.success("B.b1(\"foo\")", "b1: foo"),
        Evaluation.success("B.b2(\"foo\")", "b2: foo"),
        Evaluation.success("C.c1(\"foo\")", "c1: foo"),
        Evaluation.failed("C.c2(\"foo\")")
      )
    )
  }

  test("evaluate in private static object") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(13),
      DebugStepAssert.inParallel(
        Evaluation.success("b1", "b1"),
        Evaluation.success("b2(\"foo\")", "b2: foo"),
        Evaluation.successOrIgnore("a1", "a1", ignore = isScala2),
        Evaluation.successOrIgnore("a2(\"foo\")", "a2: foo", ignore = isScala2)
      )
    )
  }

  test("evaluate public and private fields in class") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A("a", 1)
         |    println(a)
         |  }
         |}
         |
         |class A(name: String, val n: Int) {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("a.a1", "a.a1"),
        Evaluation.success("a.B.b1", "a.B.b1"),
        Evaluation.success("new A(\"aa\", 2).a1", "aa.a1"),
        Evaluation.success("new A(\"aa\", 2).B.b1", "aa.B.b1")
      ),
      Breakpoint(23),
      DebugStepAssert.inParallel(
        Evaluation.success("name", "a"),
        Evaluation.success("n", 1),
        Evaluation.success("this.name", "a"),
        Evaluation.success("a1", "a.a1"),
        Evaluation.success("a2", "a.a2"),
        Evaluation.successOrIgnore("new A(\"aa\", 2).a2", "aa.a2", isScala2),
        Evaluation.success("B.b1", "a.B.b1"),
        Evaluation.success("this.B.b1", "a.B.b1"),
        Evaluation.success("C.c1", "a.C.c1"),
        Evaluation.success("new A(\"aa\", 2).C.c1", "aa.C.c1")
      )
    )
  }

  test("evaluate private method call in class") {
    val source =
      """|package example
         |
         |class A {
         |  val a = this
         |
         |  def foo(): String = {
         |    m("foo") // breakpoint
         |  }
         |
         |  private def m(x: String) = x
         |}
         |
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    new A().foo()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      DebugStepAssert.inParallel(
        Evaluation.success("m(\"foo\")", "foo"),
        Evaluation.success("this.m(\"bar\")", "bar"),
        Evaluation.success("a.m(\"fizz\")", "fizz")
      )
    )
  }

  test("evaluate private overloaded method") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      DebugStepAssert.inParallel(
        Evaluation.success("m()", "m"),
        Evaluation.success("m(5)", "m(5: Int)"),
        Evaluation.success("m(true)", "m(true: Boolean)"),
        Evaluation.success("m(\"foo\")", "m(foo: String)"),
        Evaluation.successOrIgnore("m(new B)", "m(b: B)", isScala2),
        Evaluation.successOrIgnore("m(new B: A)", "m(a: A)", isScala2),
        Evaluation.successOrIgnore("m(Array(1, 2))", "m(xs: Array[Int])", isScala2),
        Evaluation.successOrIgnore("m(Array[A](new B))", "m(xs: Array[A])", isScala2),
        Evaluation.successOrIgnore("m(Array(Array(1), Array(2)))", "m(xs: Array[Array[Int]])", isScala2),
        Evaluation.successOrIgnore("m1(Seq(1, 2, 3))", "List(1, 2, 3)", isScala2),
        Evaluation.successOrIgnore("m1(Vector(1, 2, 3))", "Vector(1, 2, 3)", isScala2),
        Evaluation.successOrIgnore("m1(Seq(true, false, true))", 2, isScala2)
      )
    )
  }

  test("evaluate private inner class") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(5),
      DebugStepAssert.inParallel(
        Evaluation.success("a1()")(result => assert(result.startsWith("A$B@"))),
        Evaluation.success("(new B).b1", "b1"),
        Evaluation.success("(new A.B).b2()", "b2"),
        Evaluation.success("a2(new B)", "a2")
      ),
      Breakpoint(20),
      DebugStepAssert.inParallel(
        Evaluation.success("c2()")(result => assert(result.startsWith("C$D@"))),
        Evaluation.success("(new D).d1", "d1"),
        Evaluation.success("(new this.D).d2()", "d2"),
        Evaluation.success("c3(new D)", "c3")
      )
    )
  }

  test("evaluate constructor of inner class (with captured outer)") {
    val source =
      """|package example
         |
         |object A {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    b.m()
         |  }
         |}
         |
         |class B {
         |  class C
         |  def m(): Unit = {
         |    println("m")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(6),
      Evaluation.success("new b.C")(result => assert(result.startsWith("B$C@"))),
      Breakpoint(13),
      Evaluation.success("new C")(result => assert(result.startsWith("B$C@")))
    )
  }

  test("evaluate shaded fields and values") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(11),
      Evaluation.success("x1 + x2 + x3", "ax1bx2x3"),
      Evaluation.successOrIgnore("x1 + A.this.x2 + this.x3", "ax1ax2bx3", isScala2)
    )
  }

  test("evaluate field of two-level deep outer class") {
    val source =
      """|package example
         |
         |class A {
         |  private val a = "a"
         |  class B {
         |    class C {
         |      def m(): Unit = {
         |        println(a)
         |      }
         |    }
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    val b = new a.B
         |    val c = new b.C
         |    println(c.m())
         |  }
         |}
         |
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(8), Evaluation.success("a", "a"))
  }

  test("fail evaluation of the outer class of a private final class") {
    val source =
      """|package example
         |
         |class A {
         |  val a1 = "a1"
         |  private final class B {
         |    def b1(): Unit = {
         |      println("b1")
         |    }
         |  }
         |  def a2(): Unit = {
         |    val b = new B
         |    b.b1()
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    a.a2()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      Evaluation.success("a1", if (isScala3) new NoSuchFieldException("$outer") else new NoSuchFieldError("$outer"))
    )
  }

  test("evaluate from an local class") {
    val source =
      """|package example
         |
         |class A {
         |  val x1 = "ax1"
         |  def m(): Unit = {
         |    val x1 = "x1"
         |    class B {
         |      val x2 = "bx2"
         |      def m(): Unit = {
         |        val x2 = "x2"
         |        println(x1 + A.this.x1)
         |      }
         |    }
         |    val b = new B
         |    b.m()
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    a.m()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        // B captures the local value x1
        Evaluation.successOrIgnore("new B", isScala2)(result => assert(result.startsWith("A$B$1@"))),
        // x1 is captured by B
        Evaluation.successOrIgnore("x1", "x1", isScala2),
        Evaluation.success("x2", "x2"),
        Evaluation.successOrIgnore("A.this.x1", "ax1", isScala2),
        Evaluation.successOrIgnore("this.x2", "bx2", isScala2)
      )
    )
  }

  test("evaluate nested methods") {
    val source =
      """|package example
         |
         |object A {
         |  private class B {
         |    override def toString(): String = "b"
         |  }
         |  def main(args: Array[String]): Unit = {
         |    val x1 = 1
         |    def m1(name: String): String = {
         |      s"m$x1($name)"
         |    }
         |    def m2(b: B): String = {
         |      s"m2($b)"
         |    }
         |    def m3(): B = {
         |      new B
         |    }
         |    println(m1("m") + m2(m3()))
         |    val c = new C
         |    c.m()
         |  }
         |}
         |
         |class C {
         |  val x1 = 1
         |  private class D {
         |    override def toString(): String = "d"
         |  }
         |  def m(): Unit = {
         |    def m1(name: String): String = {
         |      s"m$x1($name)"
         |    }
         |    def m2(d: D): String = {
         |      s"m2($d)"
         |    }
         |    def m3(): D = {
         |      new D
         |    }
         |    println(m1("m") + m2(m3()))
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(18),
      DebugStepAssert.inParallel(
        Evaluation.success("m1(\"x\")", "m1(x)"),
        Evaluation.success("m3()")(result => assert(result.startsWith("A$B@"))),
        Evaluation.success("m2(new B)", "m2(b)")
      ),
      Breakpoint(39),
      DebugStepAssert.inParallel(
        Evaluation.success("m1(\"x\")", "m1(x)"),
        Evaluation.success("m3()")(result => assert(result.startsWith("C$D@"))),
        Evaluation.success("m2(new D)", "m2(d)")
      )
    )
  }

  test("evaluate expression in package") {
    val source =
      """package example {
        |object Main {
        |    def main(args: Array[String]): Unit = {
        |      println("Hello, World!")
        |    }
        |  }
        |}
        |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(4), Evaluation.success("1 + 2", 3))
  }

  test("evaluate expression with Java util code") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(4), Evaluation.success("new java.util.ArrayList[String]().toString", "[]"))
  }

  test("evaluate expression inside of a lambda") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(Evaluation.success("n", 1), Evaluation.success("m1()", 9)),
      if (isScala3) Breakpoint(9) else NoStep(),
      Breakpoint(9),
      DebugStepAssert.inParallel(Evaluation.success("n", 1), Evaluation.success("m1()", 9))
    )
  }

  test("evaluate expression with breakpoint on an assignment") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.success("1 + 2", 3),
      Breakpoint(12),
      Evaluation.success("a + 2", 3)
    )
  }

  test("evaluate expression with breakpoint on method definition") {
    val source =
      """|package example
         |class Foo {
         |  def bar(): String = "foobar"
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    new Foo().bar()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(3), Evaluation.success("1 + 2", 3))
  }

  test("evaluate expression definition") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(4), Evaluation.success("val x = 123", ()))
  }

  test("evaluate multi-line expression") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = 1
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.success(
        """val b = 2
          |val c = 3
          |a + b + c
          |""".stripMargin,
        6
      )
    )
  }

  test("evaluate in default arguments") {
    val source =
      """|package example
         |object Main {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(7), Evaluation.success("x + 1", 4))
  }

  test("evaluate inside local method") {
    val source =
      """|package example
         |
         |object A {
         |  def main(args: Array[String]): Unit = {
         |    val x1 = 1
         |    def m1(name: String): String = {
         |      s"m$x1($name)"
         |    }
         |    def m2(): String = {
         |      s"m2()"
         |    }
         |    println(m1("foo"))
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(
      Breakpoint(7),
      Evaluation.success("m1(\"bar\")", "m1(bar)"),
      Evaluation.success("m2()", "m2()")
    )
  }

  test("evaluate inside multi-level nested local class and def") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): String = {
         |    val x1 = "x1"
         |    class B {
         |      def m(): String = {
         |        val x2 = "x2"
         |        def m(): String = {
         |          val x3 = "x3"
         |          class C {
         |            def m(): String = {
         |              val x4 = "x4"
         |              def m(): String = {
         |                x1 + x2 + x3 + x4
         |              }
         |              m()
         |              x1 + x2 + x3
         |            }
         |          }
         |          val c = new C
         |          c.m()
         |          x1 + x2
         |        }
         |        m()
         |        x1
         |      }
         |    }
         |    val b = new B
         |    b.m()
         |    ""
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    a.m()
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(30), // in A#m
      DebugStepAssert.inParallel(
        Evaluation.success("new B")(result => assert(result.startsWith("A$B$1@"))), // captures x1
        Evaluation.success("(new B).m()", "x1")
      ),
      Breakpoint(25), // in B#m
      DebugStepAssert.inParallel(
        Evaluation.success("x1", "x1"), // captured by B
        Evaluation.success("m()", "x1x2"), // captures x2
        Evaluation.success("this.m()", "x1"),
        Evaluation.success(
          "A.this.m()",
          if (isScala3) new NoSuchFieldException("$outer") else new NoSuchFieldError("$outer")
        ),
        Evaluation.successOrIgnore("new B", isScala2)(result => assert(result.startsWith("A$B$1@")))
      ), // captures x1
      Breakpoint(22), // in B#m#m
      DebugStepAssert.inParallel(
        Evaluation.success("x1", "x1"), // captured by B
        Evaluation.success("x2", "x2"), // captured by m
        Evaluation.success("m()", "x1x2"), // captures x2
        Evaluation.success("this.m()", "x1"), // captures x2
        Evaluation.successOrIgnore("new B", isScala2)(result => assert(result.startsWith("A$B$1@"))), // captures x1
        Evaluation.success("new C")(result => assert(result.startsWith("A$B$1$C$1@"))), // captures x2 and x3
        Evaluation.success("(new C).m()", "x1x2x3")
      ), // captures x2 and x3
      Breakpoint(17), // in C#m
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("x1", "x1", ignore = isScala2), // captured by B => $this.$outer.x1$1
        Evaluation.successOrIgnore("x2", "x2", ignore = isScala2), // captured by C => $this.x2$1
        Evaluation.successOrIgnore("x3", "x3", ignore = isScala2), // captured by C => $this.x3$1
        Evaluation.success("m()", "x1x2x3x4"), // captures x4
        Evaluation.success("this.m()", "x1x2x3"),
        Evaluation.successOrIgnore("B.this.m()", "x1", isScala2),
        // captures x2 and x3
        Evaluation.successOrIgnore("new C", isScala2)(result => assert(result.startsWith("A$B$1$C$1@"))),
        Evaluation.successOrIgnore("new B", isScala2)(result => assert(result.startsWith("A$B$1@"))), // captures x1
        Evaluation.success("new A")(result => assert(result.startsWith("A@")))
      ),
      Breakpoint(15), // in C#m#m
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("x1", "x1", ignore = isScala2), // captured by B => $this.$outer.x1$1
        Evaluation.successOrIgnore("x2", "x2", ignore = isScala2), // captured by C => $this.x2$1
        Evaluation.successOrIgnore("x3", "x3", ignore = isScala2), // captured by C => $this.x3$1
        Evaluation.success("x4", "x4"), // captured by D => local x4$1
        Evaluation.success("m()", "x1x2x3x4"), // captures x4
        Evaluation.success("this.m()", "x1x2x3"),
        Evaluation.successOrIgnore("B.this.m()", "x1", isScala2),
        Evaluation.successOrIgnore("new C", isScala2)(result =>
          assert(result.startsWith("A$B$1$C$1@"))
        ), // captures x2 and x3
        Evaluation.successOrIgnore("new B", isScala2)(result => assert(result.startsWith("A$B$1@"))), // captures x1
        Evaluation.success("new A")(result => assert(result.startsWith("A@")))
      )
    )
  }

  test("evaluate captured local variable shadowing captured variable") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = "x1"
         |    def m(): String = {
         |      println(x) // captures x = "x1"
         |      val y = {
         |        val x = "x2"
         |        val z = {
         |          val x = "x3"
         |          def m(): String = {
         |            x // captures x = "x3"
         |          }
         |          m()
         |        }
         |        z
         |      }
         |      y
         |    }
         |    println(m())
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(15), Evaluation.success("x", "x3"), Evaluation.success("m()", "x3"))
  }

  test("read and write mutable variables whose type is a value class") {
    val source =
      """|package example
         |
         |class A {
         |  private var x = 1
         |  def xx(): Int = {
         |    var y = 1
         |    var z = 1
         |    var u = 1
         |    x += 1
         |    def yy(): Int = {
         |      y += 1
         |      y
         |    }
         |    class B {
         |      def zz(): Int = {
         |        z += 1
         |        z
         |      }
         |    }
         |    val b = new B
         |    x * 100 + yy() + b.zz() + u
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a.xx())
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      DebugStepAssert.inParallel(
        Evaluation.success("x", 1),
        Evaluation.success("u", 1),
        Evaluation.success("y", 1),
        Evaluation.successOrIgnore("x = 2", (), isScala2),
        Evaluation.successOrIgnore("x", 2, isScala2),
        Evaluation.successOrIgnore("x = x - 1", (), isScala2),
        Evaluation.successOrIgnore("x", 1, isScala2),
        Evaluation.successOrIgnore("x *= 2", (), isScala2),
        Evaluation.successOrIgnore("x", 2, isScala2),
        Evaluation.successOrIgnore("u = 2; u", 2, isScala2),
        Evaluation.successOrIgnore("u", 2, isScala2),
        Evaluation.success("y += 1", ()),
        Evaluation.success("new B")(result => assert(result.startsWith("A$B$1@"))),
        Evaluation.success("yy()", 3)
      ),
      Breakpoint(11),
      // captured by method m
      DebugStepAssert.inParallel(Evaluation.success("y", 3), Evaluation.success("y += 1; y", 4)),
      Breakpoint(12),
      Evaluation.success("y", 5),
      Breakpoint(16),
      // captured by class B
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("z", 1, isScala2),
        Evaluation.successOrIgnore("z += 1; z", 2, isScala2)
      )
    )
  }

  test("evaluate lazy variables") {
    val source =
      """|package example
         |
         |object A {
         |  private lazy val x = 1
         |  def m(): Int = {
         |    lazy val y = 2
         |    x + y
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println(A.m())
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      Evaluation.successOrIgnore("x", 1, isScala2),
      if (isScala3) Evaluation.failed("y")
      else Evaluation.success("y", 2),
      Evaluation.successOrIgnore(
        """|lazy val z = 2
           |z""".stripMargin,
        2,
        isScala2
      )
    )
  }

  test("evaluate private members in parent class") {
    val source =
      """|package example
         |
         |abstract class BaseA {
         |  private val x: String = "x"
         |  private var y: String = "y"
         |  private lazy val z: String = "z"
         |  def m1: String = {
         |    val b = new B
         |    b.m3 + m2
         |  }
         |  private def m2: String = {
         |    y + z
         |  }
         |  private abstract class BaseB {
         |    def m3: String = {
         |      x
         |    }
         |  }
         |  private class B extends BaseB
         |}
         |
         |class A extends BaseA
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    println(a.m1)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("x", "x", isScala2),
        Evaluation.successOrIgnore("this.x", "x", isScala2),
        Evaluation.successOrIgnore("y", "y", isScala2),
        Evaluation.successOrIgnore("this.y = \"yy\"", (), isScala2),
        Evaluation.successOrIgnore("y", "yy", isScala2),
        Evaluation.successOrIgnore("z", "z", isScala2),
        Evaluation.successOrIgnore("m2", "yyz", isScala2)
      ),
      Breakpoint(16),
      Evaluation.success("x", "x")
    )
  }

  test("evaluate type parameter list and multi parameter lists") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |
         |  def m1[X]: String = {
         |    "m1[X]"
         |  }
         |
         |  private def m2[X]: String = {
         |    "m2[X]"
         |  }
         |
         |  private def m3(x: Int)(y: String): String = {
         |    s"m3($x)($y)"
         |  }
         |
         |  private def m4[X, Y](x: X)(y: Y): String = {
         |    s"m4($x)($y)"
         |  }
         |
         |  private class A[X]
         |  private class B(x: Int)(y: String)
         |  private class C[X, Y](x: X)(y: Y)
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      DebugStepAssert.inParallel(
        Evaluation.success("m1[String]", "m1[X]"),
        Evaluation.success("m1[A[Int]]", "m1[X]"),
        Evaluation.success("m2[String]", "m2[X]"),
        Evaluation.success("m3(1)(\"x\")", "m3(1)(x)"),
        Evaluation.successOrIgnore("m4[Int, String](1)(\"x\")", "m4(1)(x)", isScala2),
        Evaluation.success("new A[String]")(result => assert(result.startsWith("Main$A@"))),
        Evaluation.success("new B(2)(\"x\")")(result => assert(result.startsWith("Main$B@"))),
        Evaluation.success("new C[Int, String](2)(\"x\")")(result => assert(result.startsWith("Main$C@")))
      )
    )
  }

  test("evaluate instance of value class") {
    val source =
      """|package example
         |
         |object Main {
         |  var b1 = new B("foo")
         |  private var c1 = new C(2)
         |  def main(args: Array[String]): Unit = {
         |    val b2 = new B("bar")
         |    println(b1.m(c1))
         |    println(m(b1 + b2))
         |  }
         |
         |  def m(b: A): String = {
         |    val c2 = new C(5)
         |    b.m(c2)
         |  }
         |}
         |
         |trait A extends Any {
         |  def m(c: C): String
         |}
         |
         |class B(val self: String) extends AnyVal with A {
         |  def m(c: C): String = {
         |    self.take(c.size)
         |  }
         |
         |  def +(b: B): B = {
         |    new B(self + b.self)
         |  }
         |}
         |
         |class C(val size: Int) extends AnyVal
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.success("b1", "foo"),
        Evaluation.success("c1.size", 2),
        Evaluation.success("b2.m(c1)", "ba"),
        Evaluation.success("m(b2)", "bar"),
        Evaluation.success("new B(\"fizz\")", "fizz"),
        Evaluation.success("b1 + new B(\"buzz\")", "foobuzz")
      ),
      Breakpoint(24),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("self", "foo", isScala2),
        Evaluation.successOrIgnore("m(c)", "fo", isScala2)
      ),
      Breakpoint(9),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("b1 = new B(\"fizz\")", (), isScala2),
        Evaluation.successOrIgnore("c1 = new C(3)", (), isScala2)
      ),
      if (isScala2) Breakpoint(24) else NoStep(),
      Breakpoint(24),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("self", "fizzbar", isScala2),
        Evaluation.successOrIgnore("m(c)", "fizzb", isScala2)
      )
    )
  }

  test("evaluate method or constructor that takes or returns an instance of value class") {
    val source =
      """|package example
         |
         |object Main {
         |  def size: Size = new Size(1)
         |  def getMsg(size: Size): Msg = {
         |    new Msg(size)
         |  }
         |  def main(args: Array[String]): Unit = {
         |    val msg = getMsg(size)
         |    println(msg.value)
         |  }
         |}
         |
         |class Msg(size: Size) {
         |  def value: String = {
         |    "Hello, World!".take(size.value)
         |  }
         |}
         |
         |class Size(val value: Int) extends AnyVal
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("size", 1),
        Evaluation.successOrIgnore(
          """|def size2: Size = new Size(2)
             |getMsg(size2).value""".stripMargin,
          "He",
          isScala2
        ),
        Evaluation.success("new Msg(new Size(3)).value", "Hel")
      )
    )
  }

  test("evaluate captured instance of value class") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val size = new Size(2)
         |    class A(msg: String) {
         |      def m: String = {
         |        msg.take(size.value)
         |      }
         |    }
         |    def m(msg: String): String = {
         |      msg.take(size.value)
         |    }
         |    println(new A("foo").m)
         |    println(m("bar"))
         |  }
         |}
         |
         |class Size(val value: Int) extends AnyVal
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(14),
      DebugStepAssert.inParallel(
        Evaluation.success("new A(\"foo\")")(result => assert(result.startsWith("Main$A$1@"))),
        Evaluation.success("m(\"bar\")", "ba")
      ),
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("size", 2, isScala2),
        Evaluation.successOrIgnore("size.value", 2, isScala2),
        Evaluation.successOrIgnore("new A(\"foo\")", isScala2)(result => assert(result.startsWith("Main$A$1@")))
      ),
      Breakpoint(12),
      DebugStepAssert.inParallel(
        Evaluation.success("size", 2),
        Evaluation.success("size.value", 2),
        Evaluation.success("m(\"bar\")", "ba")
      )
    )
  }

  test("read and write mutable variables") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    var x: A = new A(1)
         |    var y: A = new A(1)
         |    var z: A = new A(1)
         |    z += new A(1)
         |    def xx(): A = {
         |      x += new A(1)
         |      x
         |    }
         |    class B {
         |      def yy(): A = {
         |        y += new A(1)
         |        y
         |      }
         |    }
         |    val b = new B
         |    val res = xx() + b.yy() + z
         |    println(res)
         |  }
         |}
         |
         |class A(val value: Int) extends AnyVal {
         |  def +(x: A): A = new A(value + x.value)
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.success("x", 1),
        Evaluation.success("y", 1),
        Evaluation.success("z", 1),
        Evaluation.success("x = new A(2); x", 2),
        Evaluation.success("x", 2),
        Evaluation.success("y = x + new A(1)", ()),
        Evaluation.success("y", 3),
        Evaluation.successOrIgnore("z += new A(2); z", 3, isScala2),
        Evaluation.successOrIgnore("z", 3, isScala2),
        Evaluation.success("new B")(result => assert(result.startsWith("Main$B$1@"))),
        Evaluation.success("xx()", 3)
      ),
      Breakpoint(10),
      // captured by method m
      DebugStepAssert.inParallel(Evaluation.success("x", 3), Evaluation.success("x += new A(1); x", 4)),
      Breakpoint(11),
      Evaluation.success("x", 5),
      Breakpoint(15),
      // captured by class B
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("y", 3, isScala2),
        Evaluation.successOrIgnore("y += new A(1); y", 4, isScala2)
      )
    )
  }

  test("evaluate local method in value class") {
    val source =
      """|package example
         |
         |class A(val self: String) extends AnyVal {
         |  def m(size: Int): String = {
         |    def m(mul: Int): String = {
         |      self.take(size) * mul
         |    }
         |    m(2)
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A("foo")
         |    println(a.m(2))
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("this.m(2)", "fofo", isScala2),
        Evaluation.failedOrIgnore("m(3)", "not supported", isScala2)
      ),
      Breakpoint(6),
      DebugStepAssert.inParallel(
        if (isScala3) Evaluation.failed("self", "not supported")
        else Evaluation.success("self", "foo"),
        if (isScala3) Evaluation.failed("size", "not supported")
        else Evaluation.success("size", 2),
        if (isScala3) Evaluation.failed("m(1)", "not supported")
        else Evaluation.success("m(1)", "fo"),
        if (isScala3) Evaluation.failed("this.m(1)", "not supported")
        else Evaluation.success("this.m(1)", "ff")
      )
    )
  }

  test("evaluate tail-rec function") {
    val source =
      """|package example
         |object Main {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("f(x)", 2))
  }

  test("evaluate App block method") {
    val source =
      """|package example
         |object Main extends App {
         |  val x = 1
         |  val y = {
         |    val msg = "Hello World!"
         |    println(msg)
         |    true
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("msg", "Hello World!"))
  }

  test("evaluate at lambda start") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val list = List(1, 2, 3)
         |    list.foreach { x =>
         |      println(x)
         |    }
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5), Evaluation.success("1 + 1", 2))
  }

  test("return exception as the result of evaluation") {
    val source =
      """|package example
         |object Main{
         |  def main(args: Array[String]): Unit = {
         |    println("Hello, World!")
         |  }
         |
         |  def throwException(): Unit = {
         |    throw new Exception("error")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(4),
      Evaluation.success("throwException()", new Exception("error"))
    )
  }

  test("evaluate in munit test") {
    val source =
      """|package example
         |class MySuite extends munit.FunSuite {
         |  def sum(list: List[Int]): Int = list.sum
         |
         |  test("sum of a few numbers") {
         |    assertEquals(sum(List(1,2,0)), 3)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.munitTestSuite(source, "example.MySuite", scalaVersion)
    check(
      Breakpoint(6),
      // the program stops twice before Scala 3.2
      if (!isScala31Plus) Breakpoint(6) else NoStep(),
      Evaluation.success("1 + 1", 2)
    )
  }

  test("evaluate lambdas") {
    val source =
      """|package example
         |class Foo {
         |  val a = 1
         |  private val b = 2
         |  def bar() = {
         |    val c = 3
         |    println(s"a + b + c = ${a + b + c}")
         |  }
         |}
         |
         |object Main {
         |  val a = 1
         |  private val b = 2
         |  def main(args: Array[String]): Unit = {
         |    val c = 3
         |    println(a + b + c)
         |    new Foo().bar()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(16),
      Evaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36),
      Breakpoint(7),
      DebugStepAssert.inParallel(
        Evaluation.success("List(1, 2, 3).map(_ * 2).sum", 12),
        Evaluation.success("List(1, 2, 3).map(_ * a * b * c).sum", 36)
      )
    )
  }

  test("evaluate call to anonymous function") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("f(\"foo\")", 3))
  }

  test("evaluate call to method of generic class") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.success("write(value)", ()),
      // Should it work without casting?
      // In contravariant case, we could find what's the expected type
      // In the covariant case, it is not possible to know what the precise return type is at runtime
      Evaluation.success("write(\"Hello\".asInstanceOf[T])", ())
    )
  }

  test("evaluate call to anonymous polymorphic function") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5), Evaluation.success("f(\"foo\")", "oof"))
  }

  test("evaluate local def in expression") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit =
         |    println("Hello World!")
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.successOrIgnore(
        """|def m(x: Int) = println(x)
           |m(1)""".stripMargin,
        (),
        isScala2
      ),
      Evaluation.successOrIgnore(
        "def m(x: Int) = println(x); m(1)",
        (),
        isScala2
      )
    )
  }

  test("evaluate static Java methods") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit =
         |    println("Hello, World!")
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.success(
        """|import java.nio.file.Paths
           |Paths.get(".").toString""".stripMargin,
        "."
      )
    )
  }

  test("evaluate private field in trait") {
    val source =
      """|package example
         |
         |trait A {
         |  private var a: String = "A.a"
         |
         |  def m(): Unit =
         |    println(a)
         |}
         |
         |class B extends A
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    b.m()
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      DebugStepAssert.inParallel(Evaluation.success("a", "A.a"), Evaluation.success("a = \"foo\";a", "foo"))
    )
  }

  test("encode operator symbols") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val ! = "!"
         |    println(| + new <> + &(":") + !)
         |  }
         |  private val | = "|"
         |  private class <> {
         |    override def toString(): String = "<>"
         |  }
         |  private def &(`:`: String): String = s"&(${`:`})"
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore("!", "!", isScala2),
        Evaluation.successOrIgnore("|", "|", isScala2),
        Evaluation.success("(new <>).toString", "<>"),
        Evaluation.successOrIgnore("&(\":\")", "&(:)", isScala2)
      )
    )
  }

  test("evaluate on match case") {
    val source =
      """|package example
         |
         |object Main {
         |  def m(list: List[String]): Unit = {
         |    list match {
         |      case a :: b :: tail =>
         |        println(a + b)
         |      case a :: tail => println(a)
         |      case _ => ()
         |    }
         |  }
         |
         |  def main(args: Array[String]): Unit = {
         |    m(List("a", "b", "c"))
         |    m(List("a"))
         |    m(Nil)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    if (isScala3) {
      check(
        Breakpoint(5),
        Evaluation.success("list.size", 3),
        Breakpoint(7),
        Evaluation.success("b", "b"),
        Breakpoint(5),
        Evaluation.success("list.size", 1),
        Breakpoint(8),
        Evaluation.success("list.size", 1),
        Breakpoint(5),
        Evaluation.success("list.size", 0),
        Breakpoint(9),
        Evaluation.success("list.size", 0)
      )
    } else {
      // in scala2 it stops many times on line 5 and 8
      // skipping those tests
      check(
        Breakpoint(7),
        Evaluation.success("b", "b"),
        Breakpoint(9),
        Evaluation.successOrIgnore("list.size", 0, isScala2)
      )
    }
  }

  test("evaluate on assignment") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    var msg = "Hello"
         |    msg = "Bye" + "Bye"
         |    println(msg)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("1+1", 2))
  }

  test("evaluate class def") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val msg = "Hello, World!"
         |    println(msg)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.successOrIgnore(
          """|class A(a: Int) {
             |  override def toString(): String = s"A($a)"
             |}
             |new A(1).toString""".stripMargin,
          "A(1)",
          isScala2
        ),
        Evaluation.successOrIgnore(
          """|case class A() {
             |  override def toString(): String = "A"
             |}
             |A().toString""".stripMargin,
          "A",
          isScala2
        ),
        Evaluation.successOrIgnore(
          """|class A() {
             |  def getMsg: String = msg
             |}
             |A().getMsg""".stripMargin,
          "Hello, World!",
          isScala2
        )
      )
    )
  }

  test("should unwrap exception from InvocationTargetException") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    foo(-1)
         |  }
         |
         |  private def foo(x: Int): Unit = {
         |    assert(x < 0, "not negative")
         |  }
         |}
         |
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      Evaluation.successOrIgnore("foo(1)", isScala2)(result => assert(result.contains("AssertionError@")))
    )
  }

  test("evaluate on block") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    { import Foo.msg ; println(msg) }
         |  }
         |}
         |
         |object Foo {
         |  val msg = "x"
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5), Evaluation.success("Foo.msg", "x"))
  }

  test("evaluate by-name param") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    m(true)
         |    val a = new A("foo")
         |    a.m
         |  }
         |
         |  def m(x: => Boolean): Boolean = {
         |    x
         |    def m: Boolean =
         |      x
         |    m
         |    class A {
         |      def m: Boolean =
         |        x
         |    }
         |    val a = new A
         |    a.m
         |  }
         |}
         |
         |class A(x: => String) {
         |  def m: String = {
         |    x
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(11),
      Evaluation.successOrIgnore("x", true, isScala2),
      Breakpoint(13),
      Evaluation.successOrIgnore("x", true, isScala2),
      Breakpoint(17),
      Evaluation.successOrIgnore("x", true, isScala2),
      Breakpoint(26),
      Evaluation.successOrIgnore("x", "foo", isScala2)
    )
  }

  test("ignore fatal warnings") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = "Hello"
         |    println(x)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClass(source, "example.Main", scalaVersion, Seq("-Xfatal-warnings"))
    // a pure expression does nothing in statement position
    check(Breakpoint(6), Evaluation.success("x\nx", "Hello"))
  }

  test("evaluate on for loops, generators and guards") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val list = List(1)
         |    for {
         |      x <- list
         |      y <- list
         |      z = x + y
         |    } yield x
         |    for {
         |      x <- list
         |      if x == 1
         |    } yield x
         |    for (x <- list) yield x
         |    for (x <- list) println(x)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    if (isScala3)
      check(
        Breakpoint(7),
        Evaluation.success("list(0)", 1),
        Breakpoint(8),
        DebugStepAssert.inParallel(Evaluation.success("list(0)", 1), Evaluation.success("x", 1)),
        Breakpoint(9), // calling map
        Breakpoint(9),
        Evaluation.success("x + y", 2), // finally we are into the lifted lambda x + y
        Breakpoint(8), // still in the same lifted lambda (the line position does not make any sense)
        Breakpoint(9), // again in the lifted lambda
        Breakpoint(8), // going out of the lifted lambda
        if (isScala31Plus) Breakpoint(8) else NoStep(), // regression in Scala 3.3.0
        if (isScala31Plus) Breakpoint(9) else NoStep(), // regression in Scala 3.3.0
        Breakpoint(13), // calling withFilter
        Breakpoint(13),
        Evaluation.success("x", 1),
        Breakpoint(15),
        Evaluation.success("list(0)", 1),
        Breakpoint(15),
        Breakpoint(16),
        Evaluation.success("list(0)", 1),
        Breakpoint(16)
      )
    else
      check(
        Breakpoint(7),
        Evaluation.ignore("list(0)", 1),
        Breakpoint(8),
        DebugStepAssert.inParallel(Evaluation.success("list(0)", 1), Evaluation.success("x", 1)),
        Breakpoint(9),
        Evaluation.ignore("x + y", 2),
        Breakpoint(8),
        Breakpoint(9),
        Breakpoint(8),
        Breakpoint(13),
        Evaluation.successOrIgnore("x", 1, isScala212),
        if (isScala212) Breakpoint(13) else NoStep(),
        Breakpoint(15),
        Evaluation.ignore("x", 1),
        Breakpoint(15),
        Breakpoint(16),
        Evaluation.success("list(0)", 1),
        Breakpoint(16)
      )
  }

  test("evaluate capture of pattern") {
    assume(!scalaVersion.isScala30) // Won't be fixed in Scala 3.0
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val n = 1
         |    n match {
         |      case m =>
         |        println(m)
         |        println(m)
         |    }
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    // a pure expression does nothing in statement position
    check(Breakpoint(8), Evaluation.successOrIgnore("m", 1, ignore = isScala2))
  }
}

abstract class Scala2EvaluationTests(val scalaVersion: ScalaVersion) extends ScalaEvaluationTests(scalaVersion) {
  test("should use -Xsource:3") {
    val source =
      """|package example
         |
         |import scala.{*, given}
         |open class A
         |
         |object Main {
         |  type Foo = String & Int
         |
         |  def main(args: Array[String]): Unit = {
         |    println(m(Seq("a", "b")*))
         |  }
         |
         |  def m(xs: String*): String = xs.mkString(", ")
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClass(source, "example.Main", scalaVersion, Seq("-Xsource:3"))
    check(
      Breakpoint(10),
      Evaluation.success("""m(Seq("a", "b")*)""", "a, b")
    )
  }
}

abstract class Scala3EvaluationTests(scalaVersion: ScalaVersion) extends ScalaEvaluationTests(scalaVersion) {
  test("evaluate shadowed variable") {
    val source =
      """|package example
         |object Main {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(7), Evaluation.success("foo", "bar"), Breakpoint(9), Evaluation.success("foo", "foo"))
  }

  test("brace-less syntax: public fields of objects") {
    val source =
      """|package example
         |object A:
         |  val x1 = "x1"
         |
         |object Main:
         |  val x1 = 1.1
         |  val x2 = 2.2
         |
         |  def main(args: Array[String]): Unit =
         |    println("Hello, World!")
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(10), Evaluation.success("x1 + x2")(_.contains("3.3")), Evaluation.success("A.x1", "x1"))
  }

  test("brace-less syntax: private fields of class") {
    val source =
      """|package example
         |class A:
         |  private val x1 = "x1"
         |
         |  def m1(): Unit =
         |    println(x1)
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new A().m1()
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("x1", "x1"))
  }

  test("brace-less syntax: public fields of outer class") {
    val source =
      """|package example
         |class A:
         |  val x1 = "x1"
         |  class B:
         |    val x2 = "x2"
         |    def m1(): Unit =
         |      val x3 = "x3"
         |      println(x1 + x2 + x3)
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A()
         |    val b = new a.B()
         |    b.m1()
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8), // Stops once in the constructor of B
      Breakpoint(8),
      Evaluation.success("x1 + x2 + x3", "x1x2x3")
    )
  }

  test("brace-less syntax: evaluate in package") {
    val source =
      """package example:
        |  object Main:
        |    def main(args: Array[String]): Unit =
        |      println("Hello, World!")
        |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(4),
      Evaluation.success("1 + 2", 3)
    )
  }

  test("brace-less syntax: evaluate on method definition") {
    val source =
      """|package example
         |class Foo:
         |  def bar(): String = "foobar"
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new Foo().bar()
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(3), Evaluation.success("1 + 2", 3))
  }

  test("val def in @main") {
    val source =
      """|package example
         |@main def foo(): Unit =
         |  println("Hello, World!")
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.foo", scalaVersion)
    check(Breakpoint(3), Evaluation.success("val x = 123", ()))
  }

  test("brace-less syntax: private method call") {
    val source =
      """|package example
         |class Foo:
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
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new Foo().bar()
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("this.p(\"foo\")", "foo"),
        Evaluation.success("foo.p(\"foo\")", "foo"),
        Evaluation.success("getFoo().p(\"foo\")", "foo"),
        Evaluation.success("getFoo().getFoo().p(\"foo\")", "foo"),
        Evaluation.success("A.getFoo().p(\"foo\")", "foo")
      )
    )
  }

  test("brace-less syntax: evaluate in default arguments") {
    val source =
      """|package example
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    foo(3)()
         |
         |  def foo(x: Int)(
         |    y: Int = x + 1
         |  ): Unit =
         |    println("foo")
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(7), Evaluation.success("x + 1", 4))
  }

  test("brace-less syntax: evaluate nested method") {
    val source =
      """|package example
         |class Foo:
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
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new Foo().bar()
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(13),
      DebugStepAssert.inParallel(
        Evaluation.success("msg(\"Alice\")", "Hello, Alice!"),
        Evaluation.success("msg1(1)", "Hello, 1!"),
        Evaluation.success("msg2(new Foo)", "Hello, foo!")
      )
    )
  }

  test("brace-less syntax: evaluate tail-rec function") {
    val source =
      """|package example
         |object Main:
         |  @scala.annotation.tailrec
         |  def f(x: Int): Int =
         |    if (x <= 42) then
         |      x
         |    else f(x/2)
         |
         |  def main(args: Array[String]): Unit =
         |    val result = f(2)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("f(x)", 2))
  }

  test("evaluate inline def") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("msg", "Hello, World!"),
        Evaluation.success("foo(msg, 5)", "Hello"),
        Evaluation.success("foo(msg + \"!!\", 4)", "Hell"),
        Evaluation.success("bar(foo(msg, 5))", "olleH")
      ),
      Breakpoint(6)
    )
  }

  test("evaluate macro def") {
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

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(
      Seq("Main.scala" -> mainSource, "Macro.scala" -> macroSource),
      "example.Main",
      scalaVersion
    )
    check(
      Breakpoint(8),
      DebugStepAssert.inParallel(
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

  test("evaluate expression involving enums") {
    val source =
      """|package example
         |
         |enum A(val a: Int) extends java.lang.Enum[A]:
         |  case A1 extends A(1)
         |  case A2 extends A(2)
         |
         |class B(b: String):
         |  private enum C(c: String):
         |    case C1 extends C(b)
         |    case C2(x: String) extends C(x)
         |
         |    def m: String = b + c
         |
         |  def bar: String =
         |    C.C1.m
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val b = new B("b")
         |    println(b.bar)
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(12),
      Breakpoint(12),
      Breakpoint(15),
      Breakpoint(20), // in A#m
      DebugStepAssert.inParallel(Evaluation.success("A.A1.a", 1), Evaluation.success("A.A2.a", 2)),
      Breakpoint(15),
      DebugStepAssert.inParallel(
        Evaluation.success("C.C1.m", "bb"),
        Evaluation.success("C.C2(\"bb\").m", "bbb"),
        Evaluation.success("this.C.C2(\"bb\").m", "bbb")
      ),
      Breakpoint(12),
      DebugStepAssert.inParallel(
        Evaluation.success("C1.m", "bb"),
        Evaluation.success("C2(\"bb\").m", "bbb"),
        Evaluation.success("B.this.C.C1.m", "bb"),
        Evaluation.success("C.C2(\"bb\").m", "bbb")
      )
    )
  }

  test("evaluate instance of local class in method of value class") {
    // only Scala 3 because:
    // "implementation restriction: nested class is not allowed in value class
    // This restriction is planned to be removed in subsequent releases."
    val source =
      """|package example
         |
         |class A(self: String) extends AnyVal:
         |  def m(size: Int): String =
         |    class B:
         |      def m(): String =
         |        self.take(size)
         |    val b = new B
         |    b.m()
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A("foo")
         |    println(a.m(2))
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      DebugStepAssert.inParallel(
        Evaluation.success("self", "foo"),
        Evaluation.success("m(2)", "fo"),
        Evaluation.failed("b.m()", "not supported"),
        Evaluation.failed("new B", "not supported")
      ),
      Breakpoint(7),
      DebugStepAssert.inParallel(
        Evaluation.success("1 + 1", 2),
        Evaluation.failed("self.take(size)", "not supported"),
        Evaluation.failed("m()", "not supported"),
        Evaluation.failed("new B", "not supported"),
        Evaluation.failed("A.this", "not supported")
      )
    )
  }

  test("should use explicit nulls") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val classLoader = getClass.getClassLoader
         |    println(classLoader.toString)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClass(source, "example.Main", scalaVersion, Seq("-Yexplicit-nulls"))
    check(
      Breakpoint(6),
      Evaluation.failed(
        "classLoader.loadClass(\"java.lang.String\")",
        "not a member of ClassLoader | Null"
      )
    )
  }
}
