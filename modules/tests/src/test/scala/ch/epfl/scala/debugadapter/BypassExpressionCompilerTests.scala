package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class BypassExpressionCompilerTests extends DebugTestSuite {
  private val scalaVersion = ScalaVersion.`3.2`
  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.AlwaysBypassCompiler)

  test("evaluate local values") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    m("hello")
         |  }
         |
         |  def m(str: String): String = {
         |    val x = 1
         |    var y = 2
         |    lazy val z = 3
         |    str * (x + z - y)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(12),
      Evaluation.success("str", "hello"),
      Evaluation.success("x", 1),
      Evaluation.success("y", 2),
      Evaluation.failed("z")
    )
  }

  test("evaluate argument of a lambda") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    List(1).foreach { n =>
         |      println(n)
         |    }
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Breakpoint(6), Evaluation.success("n", 1))
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
    check(Breakpoint(7), Evaluation.success("x", 3))
  }

  test("evaluate inside local method") {
    val source =
      """|package example
         |
         |object A {
         |  def main(args: Array[String]): Unit = {
         |    val x1 = 1
         |    var x2 = 2
         |    def m: String = {
         |      s"$x1 $x2"
         |    }
         |    println(m)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    check(Breakpoint(8), Evaluation.success("x1$1", 1), Evaluation.success("x2$1", 2))
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
    check(Breakpoint(15), Evaluation.success("x", "x3"))
  }

  test("read and write mutable variables whose type is a value class") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    m()
         |  }
         |
         |  def m(): Int = {
         |    var y = 1
         |    var u = 1
         |    def yy(): Int = {
         |      y += 1
         |      y
         |    }
         |    yy() + u
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(15),
      Evaluation.success("y", 1),
      Evaluation.success("u", 1),
      Breakpoint(13),
      // captured by method m
      Evaluation.success("y$1", 2)
    )
  }

  test("evaluate instance of value class") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B("foo")
         |    println(m(b))
         |  }
         |
         |  def m(a: A): String = {
         |    val c = new C(5)
         |    a.m(c)
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
         |}
         |
         |class C(val size: Int) extends AnyVal
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      Evaluation.success("b", "foo"),
      Breakpoint(11),
      Evaluation.success("a", ObjectRef("B")),
      Evaluation.success("c", 5),
      Breakpoint(21),
      Evaluation.success("c", 5),
      Evaluation.success("$this", "foo")
    )
  }

  test("evaluate captured instance of value class") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val size = new Size(2)
         |    def m: Int = {
         |      size.value
         |    }
         |    println(m)
         |  }
         |}
         |
         |class Size(val value: Int) extends AnyVal
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(7), Evaluation.success("size$1", 2))
  }

  test("read and write mutable variables of value class type") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    var x: A = new A(1)
         |    var z: A = new A(1)
         |    z += new A(1)
         |    def xx(): A = {
         |      x += new A(1)
         |      x
         |    }
         |    xx()
         |  }
         |}
         |
         |class A(val value: Int) extends AnyVal {
         |  def +(x: A): A = new A(value + x.value)
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      Evaluation.success("x", 1),
      Evaluation.success("z", 1),
      Breakpoint(12),
      Evaluation.success("z", 2),
      Breakpoint(10),
      Evaluation.success("x$1", 2)
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
    check(Breakpoint(6), Evaluation.success("x", 2))
  }

  test("encode operator symbols") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val ! = "!"
         |    println(!)
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), Evaluation.success("!", "!"))
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
         |      case _ => ()
         |    }
         |  }
         |
         |  def main(args: Array[String]): Unit = {
         |    m(List("a", "b", "c"))
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(7), Evaluation.success("b", "b"))
  }

  test("by-name param vs Function0") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    m1(true)
         |    m2(() => false)
         |  }
         |
         |  def m1(x: => Boolean): Boolean =
         |    x
         |
         |  def m2(x: () => Boolean): Boolean =
         |    x()
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    // the evaluator refuses to bypass the compiler because it does not know
    // if x is a Function0 or a by-name param
    check(Breakpoint(10), Evaluation.failed("x"), Breakpoint(13), Evaluation.failed("x"))
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
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      Evaluation.success("x", 1),
      Breakpoint(9), // calling map
      Breakpoint(9),
      Evaluation.success("y", 1), // finally we are into the lifted lambda x + y
      Breakpoint(8), // still in the same lifted lambda (the line position does not make any sense)
      Breakpoint(9), // again in the lifted lambda
      Breakpoint(8), // going out of the lifted lambda
      Breakpoint(8), // regression in Scala 3.2.1
      Breakpoint(9), // regression in Scala 3.2.1
      Breakpoint(13), // calling withFilter
      Breakpoint(13),
      Evaluation.success("x", 1)
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
    check(Breakpoint(8), Evaluation.success("m", 1))
  }

  test("evaluate in Java file") {
    val source =
      """|package example;
         |
         |class Main {
         |  public static void main(String[] args) {
         |    Main main = new Main();
         |    main.greet("World");
         |  }
         |
         |  public void greet(String name) {
         |    System.out.println("Hello " + name);
         |  }
         |}
         |
         |""".stripMargin
    implicit val debuggee = TestingDebuggee.fromJavaSource(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      Evaluation.success("main", ObjectRef("Main")),
      Breakpoint(10),
      Evaluation.success("name", "World")
    )
  }
}
