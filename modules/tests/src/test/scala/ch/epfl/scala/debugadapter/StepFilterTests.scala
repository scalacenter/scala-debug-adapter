package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class Scala3StepFilterTests extends StepFilterTests(ScalaVersion.`3.1+`) {

  test("step into method with @targetName") {
    val source =
      """|package example
         |
         |import scala.annotation.targetName
         |
         |object Main {
         |  def main(args: Array[String]): Unit =
         |    m("Hello")
         |
         |  @targetName("mTarget")
         |  def m(message: String): Unit =
         |    println(message)
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(7), StepIn.method(if (isScala3) "Main.m(message: String): Unit" else "Main$.mTarget(String)"))
  }

  test("given lazy val") {
    val source =
      """|package example
         |
         |trait Msg {
         |  def value: String
         |}
         |
         |object Msg {
         |  val default = new Msg {
         |    def value: String = "Hello"
         |  }
         |  def greet(using msg: Msg): Unit = println(msg.value)
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    m
         |    A.m
         |    B.m
         |  }
         |
         |  def m: Unit =
         |    given Msg = Msg.default
         |    Msg.greet
         |    Msg.greet
         |}
         |
         |object A {
         |  given Msg = Msg.default
         |  def m =
         |    Msg.greet
         |    Msg.greet
         |}
         |
         |object B {
         |  given Msg with
         |    def value: String = "Hello"
         |  def m =
         |    Msg.greet
         |    Msg.greet
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(23),
      StepIn.line(22),
      StepIn.line(23),
      StepIn.line(11),
      StepOut.line(24),
      StepIn.line(11),
      Breakpoint(30),
      StepIn.line(28),
      StepIn.line(30),
      StepIn.line(11),
      StepOut.line(31),
      StepIn.line(11),
      Breakpoint(38),
      StepIn.line(11),
      StepOut.line(39),
      StepIn.line(11)
    )
  }
}

class Scala212StepFilterTests extends StepFilterTests(ScalaVersion.`2.12`)
class Scala213StepFilterTests extends StepFilterTests(ScalaVersion.`2.13`) {

  test("should match all kinds of Scala 2 types (not valid in Scala 3)") {
    val source =
      """|package example
         |
         |trait A {
         |  class B
         |}
         |
         |object Main extends A {
         |  class B
         |  def m(b: Main.super[A].B): Main.super[A].B = b
         |  def m(x: Either[Int, X] forSome { type X }): Either[Y, Int] forSome { type Y } = x.swap
         |
         |  def main(args: Array[String]): Unit = {
         |    val b0: super[A].B = new super[A].B
         |    m(b0)
         |    val x = Right(2)
         |    m(x)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`2.13`)
    check(Breakpoint(14), StepIn.line(9), Breakpoint(16), StepIn.line(10))
  }
}

abstract class StepFilterTests(protected val scalaVersion: ScalaVersion) extends DebugTestSuite {
  private val printlnMethod =
    if (scalaVersion.isScala3) "Predef.println(x: Any): Unit" else "Predef$.println(Object): void"
  test("should not step into mixin forwarder") {
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
         |    b.m()
         |    val c = new C
         |    c.m()
         |    val d = new D
         |    d.m()
         |    val e = new E
         |    e.m()
         |    F.m()
         |    val g = new G
         |    g.m()
         |    val h = new H
         |    h.m()
         |    val a1 = new A {}
         |    a1.m()
         |    val a2 = new A {
         |      override def m(): String = "g.m()"
         |    }
         |    a2.m()
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    // TODO use testMode after resolving ambiguity on local classes
    check(config = defaultConfig.copy(testMode = false))(
      Breakpoint(10),
      StepIn.line(4),
      StepOut.line(10),
      Breakpoint(12),
      StepIn.line(4),
      StepOut.line(12),
      Breakpoint(14),
      StepIn.line(44),
      StepOut.line(14),
      Breakpoint(16),
      StepIn.line(44),
      StepOut.line(16),
      Breakpoint(17),
      StepIn.line(4),
      StepOut.line(17),
      Breakpoint(19),
      StepIn.line(31),
      StepOut.line(19),
      Breakpoint(21),
      StepIn.line(4),
      StepOut.line(21),
      Breakpoint(23),
      if (isScala3) StepIn.line(4)
      else StepIn.line(22), // cannot skip method in local class
      StepOut.line(23),
      Breakpoint(27),
      StepIn.line(25),
      StepOut.line(27)
    )
  }

  test("step into local class or local object") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]) = {
         |    class A {
         |      def m(): Unit = {
         |        println("A.m")
         |      }
         |    }
         |    object B {
         |      def m(): Unit = {
         |        println("B.m")
         |      }
         |    }
         |    val a = new A
         |    a.m()
         |    B.m()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(16),
      StepIn.line(7),
      // object B becomes a lazy ref at runtime, to avoid all the steps, it just breaks in B.m()
      Breakpoint(12)
    )
  }

  test("should not step into getters") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(14),
      StepIn.line(10),
      Breakpoint(15),
      StepIn.line(10),
      Breakpoint(16),
      StepIn.line(10),
      Breakpoint(17),
      StepIn.line(10),
      Breakpoint(43),
      StepIn.line(10),
      Breakpoint(44),
      StepIn.line(10),
      Breakpoint(45),
      StepIn.line(10),
      Breakpoint(46),
      StepIn.line(10),
      Breakpoint(47),
      StepIn.line(10),
      Breakpoint(48),
      StepIn.line(10),
      Breakpoint(49),
      StepIn.line(10),
      Breakpoint(23),
      StepIn.line(10)
    )
  }

  test("should not step into setters") {
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
         |  /* override */ var a1: String = "a1"
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      StepIn.line(9),
      StepIn.line(11),
      Breakpoint(30),
      StepIn.line(31),
      StepIn.line(32),
      StepIn.line(33),
      StepIn.line(34),
      StepIn.line(35),
      StepIn.line(12)
    )
  }

  test("should not step into bridges") {
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
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(14), StepIn.line(8), StepOut.line(14))
  }

  test("should step into methods of value classes") {
    val source =
      """|package example
         |
         |class A(val x: String) extends AnyVal {
         |  def m(): String = {
         |    x + x
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a: A = new A("x")
         |    println(a.m())
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(12), StepIn.line(5), StepOut.line(12))
  }

  test("should step into methods with several parameter lists") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    m()(a)
         |  }
         |
         |  def m()(a: A): String = {
         |    a.toString
         |  }
         |}
         |
         |class A {
         |  override def toString(): String = "B"
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(6), StepIn.line(10), StepIn.line(15), StepOut.line(10), StepOut.line(6))
  }

  test("should step into lazy initializer") {
    val source =
      """|package example
         |
         |object A extends B {
         |  lazy val a = {
         |    "a".toString
         |  }
         |
         |  def main(args: Array[String]): Unit = {
         |    println(a)
         |    println(a)
         |    println(b)
         |    println(b)
         |  }
         |}
         |
         |trait B {
         |  lazy val b = {
         |    "b".toString
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.A", scalaVersion)
    if (isScala3) {
      // This only works since Scala 3.3.0: in previous versions, there is a single method for
      // getting and initializing the lazy field. We skip it.
      check(config = defaultConfig.copy(testMode = false))(
        Breakpoint(9),
        StepIn.line(4),
        StepIn.line(6),
        StepIn.line(4),
        StepIn.line(5),
        StepIn.method("String.toString(): String"),
        StepIn.line(4),
        StepIn.line(6),
        StepIn.line(9),
        StepIn.method(printlnMethod),
        Breakpoint(10),
        StepIn.method(printlnMethod),
        Breakpoint(11),
        StepIn.line(18),
        StepIn.method("String.toString(): String"),
        StepIn.line(18),
        StepIn.line(11),
        StepIn.method(printlnMethod),
        Breakpoint(12),
        StepIn.method(printlnMethod)
      )
    } else {
      check(
        Breakpoint(9),
        StepIn.line(4),
        StepIn.line(5),
        StepOut.line(9),
        StepIn.method(printlnMethod),
        Breakpoint(10),
        StepIn.method(printlnMethod),
        Breakpoint(11),
        StepIn.line(18),
        StepOut.line(11),
        StepIn.method(printlnMethod),
        Breakpoint(12),
        StepIn.method(printlnMethod)
      )
    }
  }

  test("should not step into synthetic method of case class") {
    val source =
      """|package example
         |
         |case class A(a: String)
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A("a")
         |    val b = new A("b")
         |    
         |    a.toString
         |    a.copy("b")
         |    a.hashCode()
         |    a.equals(b)
         |    a.productArity
         |    a.productPrefix
         |    a.productElement(0)
         |    a.productIterator
         |    
         |    val f = A
         |    f("a")
         |
         |    val x = A.unapply(a)
         |  }
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(10),
      StepIn.method(
        if (isScala3) "ScalaRunTime._toString(x: Product): String"
        else "ScalaRunTime$._toString(Product): String"
      ),
      StepOut.line(10),
      Breakpoint(11),
      StepIn.method(if (isScala3) "A.<init>(a: String): Unit" else "A.<init>(String): void"),
      StepOut.line(11),
      Breakpoint(12),
      StepIn.method(
        if (isScala3) "ScalaRunTime._hashCode(x: Product): Int" else "ScalaRunTime$._hashCode(Product): int"
      ),
      StepOut.line(12),
      Breakpoint(13),
      StepIn.method("String.equals(Object): boolean"),
      StepOut.line(13),
      Breakpoint(14),
      StepIn.line(15),
      StepIn.line(16),
      StepIn.line(17),
      if (isScala2) StepIn.method("ScalaRunTime$.typedProductIterator(Product): Iterator")
      else StepIn.method(if (isScala3) "Product.productIterator: Iterator[Any]" else "Product.productIterator()")
    )
  }

  test("should step in and out of a lambda") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val f = (x: Int) => x + 3
         |    f(3)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5), StepIn.line(6), StepIn.line(5), StepOut.line(6))
  }

  test("should step in method that return this.type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): this.type = this
         |}
         |
         |class B extends A
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    a.m()
         |
         |    val b = new B
         |    b.m()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(12), StepIn.line(4), Breakpoint(15), StepIn.line(4))
  }

  test("should step into default value method") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    new A; new B
         |    m("foo")
         |    A()
         |    new B()
         |  }
         |
         |  def m(x: String = "", y: Int = 2): String = {
         |    x * 2
         |  }
         |}
         |
         |case class A(a1: String = "", y: Int = 2)
         |
         |class B(b: String = B.default)
         |
         |object B {
         | def default: String = "b"
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      StepIn.line(11),
      StepIn.line(6),
      StepIn.line(12),
      Breakpoint(7),
      StepIn.line(16),
      StepIn.line(7),
      StepIn.line(16),
      StepIn.line(7),
      StepIn.line(16),
      Breakpoint(8),
      StepIn.line(18),
      StepIn.line(21),
      StepOut.line(18),
      StepOut.line(8),
      StepIn.method(if (isScala3) "B.<init>(b: String): Unit" else "B.<init>(String): void")
    )
  }

  test("should match method on parameters and return type") {
    val source =
      """|package example
         |
         |trait A {
         |  def m(xs: List[Int]): Int = xs.sum
         |}
         |
         |class B extends A {
         |  def m(xs: List[String]): String = xs.mkString(", ")
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    val strings = List("a", "b", "c")
         |    val ints = List(1, 2, 3)
         |    b.m(strings)
         |    b.m(ints)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(16), StepIn.line(8), Breakpoint(17), StepIn.line(4))
  }

  test("should match all kinds of types") {
    // To determine if a method should be stepped into, the ScalaStepFilter tries to find the Scala method
    // whose signature could match the one of the runtime method, from the class file.
    // In this test we check that all kind of Scala types can match their Java counterpart.
    val source =
      """|package example
         |
         |class annot
         |  extends scala.annotation.Annotation
         |  with scala.annotation.StaticAnnotation
         |
         |trait A {
         |  class B
         |}
         |
         |object Main extends A {
         |  def m(a : example.A): example.A = a
         |  def mbis(b: A#B): A#B = b
         |  def mbis(a: A)(b: a.B): a.B = b
         |  def m(a: this.type): this.type = a
         |  def mbis(a: A { def b: B }): A { def b: B } = a
         |  def m(x: String @annot): String @annot = x
         |  def m[T](x: T): T = x
         |  def mbis(a: Main.type): Main.type = a
         |
         |  def main(args: Array[String]): Unit = {
         |    m(Main: A): A
         |    val b0: B = new B
         |    mbis(b0)
         |    val a1: A = Main
         |    val b1: a1.B = new a1.B
         |    mbis(a1)(b1)
         |    m(Main)
         |    val a2: A { def b: B } = new A { def b: B = new B }
         |    mbis(a2)
         |    m("foo")
         |    m(5)
         |    mbis(Main)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(22),
      StepIn.line(12),
      Breakpoint(24),
      StepIn.line(13),
      Breakpoint(27),
      StepIn.line(14),
      Breakpoint(28),
      StepIn.line(15),
      Breakpoint(30),
      StepIn.line(16),
      Breakpoint(31),
      StepIn.line(17),
      Breakpoint(32),
      StepIn.method("BoxesRunTime.boxToInteger(int): Integer"),
      StepOut.line(32),
      StepIn.line(18),
      Breakpoint(33),
      StepIn.line(19)
    )
  }

  test("step into method with constant result type") {
    if (scalaVersion.isScala213 || scalaVersion.isScala3) {
      val source =
        """|package example
           |
           |object Main {
           |  def m(x: "a"): 1 = 1
           |  
           |  def main(args: Array[String]): Unit = {
           |    m("a")
           |  }
           |}
           |""".stripMargin
      implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

      check(Breakpoint(7), StepIn.line(4))
    }
  }

  test("step into method with alias result type") {
    val source =
      """|package example
         |
         |class A
         |
         |object Main {
         |  type Foo = A
         |  type Bar = String
         |  def m(x: Foo): Bar  = x.toString
         |  
         |  def main(args: Array[String]): Unit = {
         |    val foo = new A
         |    m(foo)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(12), StepIn.line(8))
  }

  test("step into method with refined result type") {
    val source =
      """|package example
         |
         |trait A
         |trait B extends A
         |
         |object Main {
         |  def m1(): A with B { def foo: String }  = {
         |    new A with B { def foo: String = toString }
         |  }
         |  
         |  def m2(): { def foo: String } = {
         |    new { def foo: String = toString }
         |  }
         |  
         |  def main(args: Array[String]): Unit = {
         |    m1()
         |    m2()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(16), StepIn.line(8), Breakpoint(17), StepIn.line(12))
  }

  test("step into method with parametric result type") {
    val source =
      """|package example
         |
         |class A
         |
         |trait B {
         |  type X <: A
         |  
         |  def m1(x: X): X = x
         |  def m2[T <: X](x: T) = x  
         |}
         |
         |class C extends B {
         |  type X = A
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val c = new C
         |    val a = new A
         |    c.m1(a)
         |    c.m2(a)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(20), StepIn.line(8), Breakpoint(21), StepIn.line(9))
  }

  test("step into method with external nested class") {
    val source =
      """|package example
         |
         |object WeekDay extends Enumeration {
         |  type WeekDay = Value
         |  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
         |}
         |
         |object Main {
         |  def today(): Enumeration#Value = WeekDay.Mon
         |
         |  def main(args: Array[String]): Unit = {
         |    today()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(12), StepIn.line(9))
  }

  test("step into method with external nested class") {
    val source =
      """|package example
         |
         |object WeekDay extends Enumeration {
         |  type WeekDay = Value
         |  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
         |}
         |
         |object Main {
         |  def today(): Enumeration#Value = WeekDay.Mon
         |
         |  def main(args: Array[String]): Unit = {
         |    today()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(12), StepIn.line(9))
  }

  test("should match Null and Nothing") {
    val source =
      """|package example
         |
         |object Main {
         |  def m(xs: Array[Int]): Nothing = ???
         |  def m(xs: Array[String]): Null = null
         |
         |  def main(args: Array[String]): Unit = {
         |    val ints = Array(1, 2)
         |    m(args)
         |    m(ints)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(9), StepIn.line(5), Breakpoint(10), StepIn.line(4))
  }

  test("should catch ClassNotLoadedException") {
    val source =
      """|package example
         |
         |class Foo
         |object Foo {
         |  def m: Foo = new Foo
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    Foo.m
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(10), StepIn.line(5))
  }

  test("should match on Array[T] whose erasure is Object") {
    val source =
      """|package example
         |
         |object Main {
         |  def m[T](xs: Array[T]): Array[T] = xs
         |
         |  def main(args: Array[String]): Unit = {
         |    val xs = Array("a", "b")
         |    m(xs)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(8), StepIn.line(4))
  }

  test("should match on PolyType") {
    val source =
      """|package example
         |
         |class A[B[_]] {
         |  def m[T](x: B[T]): B[T] = x
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a1 = new A[List]
         |    val a2 = new A[Array]
         |    val list = List("")
         |    val array = Array(1)
         |    a1.m(list)
         |    a2.m(array)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(13), StepIn.line(4), Breakpoint(14), StepIn.line(4))
  }

  test("should step into trait initializer, if it is not empty") {
    val source =
      """|package example
         |
         |trait A {
         |  val a: String = "a"
         |}
         |
         |trait B {
         |  lazy val b: String  = "b"
         |}
         |
         |trait C {
         |  def c: String = "c"
         |}
         |
         |class D extends A with B with C
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val d = new D
         |    d.a
         |    d.b
         |    d.b
         |    d.c
         |  }
         |}
         |
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    if (isScala3) {
      check(config = defaultConfig.copy(testMode = false))(
        Breakpoint(19),
        StepIn.method("D.<init>(): Unit"),
        StepIn.method("Object.<init>(): void"),
        StepIn.method("D.<init>(): Unit"),
        StepIn.method("A.<init>(): Unit"),
        StepIn.method("D.<init>(): Unit"),
        StepIn.method("Statics.releaseFence(): void"),
        Breakpoint(21),
        StepIn.line(8),
        Breakpoint(22),
        StepIn.line(23),
        StepIn.line(12)
      )
    } else {
      check(
        Breakpoint(19),
        StepIn.method("D.<init>(): void"),
        StepIn.method("Object.<init>(): void"),
        StepIn.method("D.<init>(): void"),
        StepIn.method("A.$init$(A): void"),
        StepIn.method("A.$init$(A): void"),
        StepIn.method("D.<init>(): void"),
        if (isScala213) StepIn.method("Statics.releaseFence(): void")
        else StepIn.line(19),
        Breakpoint(20),
        StepIn.line(21),
        StepIn.line(8),
        Breakpoint(22),
        StepIn.line(23),
        StepIn.line(12)
      )
    }
  }

  test("should match on vararg type") {
    val source =
      """|package example
         |
         |case class A(as: String*)
         |
         |object Main {
         |  def main(xs: Array[String]): Unit = {
         |    val parts = Seq("x", "y")
         |    val a = A(parts:_*)
         |    println(a.as)
         |    val sc = StringContext(parts:_*)
         |    println(sc.parts)
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(8),
      StepIn.method(if (isScala3) "A.<init>(as: String*): Unit" else "A.<init>(Seq): void"),
      Breakpoint(9),
      StepIn.method(printlnMethod),
      Breakpoint(10),
      StepIn.method(
        if (isScala3) "StringContext.<init>(parts: String*): Unit"
        else "StringContext.<init>(Seq): void"
      ),
      Breakpoint(11),
      StepIn.method(printlnMethod)
    )
  }

  test("encode operator symbols") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = new <>
         |    x.m
         |    &(x)
         |  }
         |  
         |  def &(x: <>): String = x.toString
         |}
         |
         |class <> {
         |  def m: <> = this
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      StepIn.method(if (isScala3) "<>.<init>(): Unit" else "$less$greater.<init>(): void"),
      Breakpoint(6),
      StepIn.method(if (isScala3) "<>.m: <>" else "$less$greater.m(): $less$greater"),
      Breakpoint(7),
      StepIn.method(if (isScala3) "Main.&(x: <>): String" else "Main$.$amp($less$greater): String")
    )
  }

  test("should step into local recursive method") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(xs: Array[String]): Unit = {
         |    fac(2)
         |  }
         |  
         |  def fac(x: Int): Int = {
         |    def rec(x: Int, acc: Int): Int = {
         |      if (x <= 0) acc
         |      else rec(x - 1, acc * x)
         |    }
         |    rec(x, 1)
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(5),
      StepIn.line(13),
      StepIn.line(10),
      StepIn.line(11),
      StepIn.line(10),
      StepOut.line(13),
      StepOut.line(5)
    )
  }

  test("should step into local lazy initializer") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    lazy val foo = {
         |      println("foo")
         |      "foo"
         |    }
         |    println(foo)
         |    println(foo)
         |  }
         |}
         |""".stripMargin

    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      StepIn.method(if (isScala3) "Main.main.foo.<lazy init>: String" else "Main$.foo$lzycompute$1(LazyRef): String"),
      StepOut.line(9),
      Breakpoint(10),
      StepIn.method(printlnMethod)
    )
  }

  test("skip class loader") {
    val source =
      """|package example
         |
         |class A
         |class B
         |class C
         |class D
         |class E
         |class F
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a: Any = new A
         |    new Array[B](1)
         |    Array.ofDim[C](1, 2)
         |    println(classOf[D])
         |    a match {
         |      case e: E => ???
         |      case _ => println("ok")
         |    }
         |    a.asInstanceOf[F]
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    if (isScala3)
      check(
        Breakpoint(12),
        StepIn.method("A.<init>(): Unit"),
        Breakpoint(13),
        StepIn.line(14),
        StepIn.line(15),
        StepIn.method(printlnMethod),
        Breakpoint(17),
        StepIn.line(18),
        Breakpoint(20),
        StepIn.method("ClassCastException.<init>(String): void")
      )
    else
      check(
        Breakpoint(12),
        StepIn.method("A.<init>(): void"),
        Breakpoint(13),
        StepIn.line(14),
        StepIn.method("ClassTag$.apply(Class): ClassTag"),
        Breakpoint(15),
        StepIn.method(printlnMethod),
        Breakpoint(17),
        StepIn.line(18),
        Breakpoint(20),
        StepIn.method("ClassCastException.<init>(String): void")
      )
  }

  test("step in private method of outer class") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val outer = new Outer
         |    val inner = new outer.Inner
         |    println(inner.bar)
         |  }
         |}
         |
         |class Outer {
         |  private def foo: String = "foo"
         |  class Inner {
         |    def bar: String = {
         |      foo
         |    }
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(15), StepIn.line(12))
  }

  test("step in method of local class with operator symbols") {
    val source =
      """|package example
         |
         |class `A+B` {
         |  val foo = 42
         |  object && {
         |    def x = {
         |      println(foo)
         |      42
         |    }
         |  }
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val ab = new `A+B`
         |    println(ab.&&.x)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(8), Evaluation.success("foo", 42))
  }

  test("skip private accessor") {
    val source =
      """|package example
         |
         |final case class A(private val x: String)
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a1 = new A("a1")
         |    val a2 = new A("a2")
         |    a1 == a2
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(9), StepIn.method("String.equals(Object): boolean"))
  }

  test("skip wrapRefArray") {
    val source =
      """|package example
         |
         |object Main {
         |  def m1(xs: String*) = println(xs.mkString)
         |  def m2(xs: Int*) = println(xs.mkString)
         |  def m3(xs: Unit*) = println(xs.mkString)
         | 
         |  def main(args: Array[String]): Unit = {
         |    m1("a", "b")
         |    m2(1, 2)
         |    m3((), ())
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      StepIn.line(4),
      Breakpoint(10),
      StepIn.line(5),
      Breakpoint(11),
      StepIn.line(6)
    )
  }
}
