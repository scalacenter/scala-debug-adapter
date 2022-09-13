package ch.epfl.scala.debugadapter

import utest._

object Scala212StepFilterTests extends StepFilterTests(ScalaVersion.`2.12`)
object Scala213StepFilterTests extends StepFilterTests(ScalaVersion.`2.13`)
object Scala3StepFilterTests extends StepFilterTests(ScalaVersion.`3.2`)

abstract class StepFilterTests(scalaVersion: ScalaVersion)
    extends StepFilterSuite(scalaVersion) {

  def tests: Tests = Tests {
    "should not step into mixin forwarder" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(10)(StepInto.line(4), StepOut.line(10)),
        Breakpoint(12)(StepInto.line(4), StepOut.line(12)),
        Breakpoint(14)(StepInto.line(44), StepOut.line(14)),
        Breakpoint(16)(StepInto.line(44), StepOut.line(16)),
        Breakpoint(17)(StepInto.line(4), StepOut.line(17)),
        Breakpoint(19)(StepInto.line(31), StepOut.line(19)),
        Breakpoint(21)(StepInto.line(4), StepOut.line(21)),
        Breakpoint(23)(
          if (isScala3) StepInto.line(4)
          else StepInto.line(22), // cannot skip method in local class
          StepOut.line(23)
        ),
        Breakpoint(27)(StepInto.line(25), StepOut.line(27))
      )
    }

    "should not step into getters" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(14)(StepInto.line(10)),
        Breakpoint(15)(StepInto.line(10)),
        Breakpoint(16)(StepInto.line(10)),
        Breakpoint(17)(StepInto.line(10)),
        Breakpoint(43)(StepInto.line(10)),
        Breakpoint(44)(StepInto.line(10)),
        Breakpoint(45)(StepInto.line(10)),
        Breakpoint(46)(StepInto.line(10)),
        Breakpoint(47)(StepInto.line(10)),
        Breakpoint(48)(StepInto.line(10)),
        Breakpoint(49)(StepInto.line(10)),
        Breakpoint(23)(StepInto.line(10))
      )
    }

    "should not step into setters" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(8)(
          StepInto.line(9),
          StepInto.line(11)
        ),
        Breakpoint(30)(
          StepInto.line(31),
          StepInto.line(32),
          StepInto.line(33),
          StepInto.line(34),
          StepInto.line(35),
          StepInto.line(12)
        )
      )
    }

    "should not step into bridges" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(14)(StepInto.line(8), StepOut.line(14))
      )
    }

    "should step into methods of value classes" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(12)(StepInto.line(5), StepOut.line(12))
      )
    }

    "should step into methods with several parameter lists" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(
          StepInto.line(10),
          StepInto.line(15),
          StepOut.line(10),
          StepOut.line(6)
        )
      )
    }

    "should step into lazy initializer" - {
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

      val breakpoints = if (isScala3) {
        Seq(
          // TODO: clean debug line table in Scala 3 compiler
          // TODO: introduce $lazyinit$ to isolate user code
          Breakpoint(9)(
            StepInto.line(4),
            StepInto.line(6),
            StepInto.line(4),
            StepInto.line(6),
            StepInto.line(5),
            StepOut.line(9),
            StepInto.method("Predef$.println(Object)")
          ),
          Breakpoint(10)(
            StepInto.line(4),
            StepInto.line(6),
            StepInto.line(4),
            StepInto.line(6),
            StepOut.line(10),
            StepInto.method("Predef$.println(Object)")
          ),
          Breakpoint(11)(
            StepInto.line(18),
            StepOut.line(11),
            StepInto.method("Predef$.println(Object)")
          ),
          Breakpoint(12)(StepInto.method("Predef$.println(Object)"))
        )
      } else {
        Seq(
          Breakpoint(9)(
            StepInto.line(4),
            StepInto.line(5),
            StepOut.line(9),
            StepInto.method("Predef$.println(Object)")
          ),
          Breakpoint(10)(StepInto.method("Predef$.println(Object)")),
          Breakpoint(11)(
            StepInto.line(18),
            StepOut.line(11),
            StepInto.method("Predef$.println(Object)")
          ),
          Breakpoint(12)(StepInto.method("Predef$.println(Object)"))
        )
      }
      assertInMainClass(source, "example.A")(breakpoints: _*)
    }

    "should not step into synthetic method of case class" - {
      val source =
        """|package example
           |
           |case class A(a: String)
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    val a = new A("a")
           |    val b = new A("b")
           |    a.productIterator // load a bunch of classes 
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(11)(
          StepInto.method("ScalaRunTime$._toString(Product)"),
          StepOut.line(11)
        ),
        Breakpoint(12)(StepInto.method("A.<init>(String)"), StepOut.line(12)),
        Breakpoint(13)(
          StepInto.method("ScalaRunTime$._hashCode(Product)"),
          StepOut.line(13)
        ),
        Breakpoint(14)(
          StepInto.method("String.equals(Object)"),
          StepOut.line(14)
        ),
        Breakpoint(15)(
          StepInto.line(16),
          StepInto.line(17),
          StepInto.line(18),
          if (isScala3) StepInto.method("Product.productIterator()")
          else StepInto.method("ScalaRunTime$.typedProductIterator(Product)")
        )
      )
    }

    "should step in and out of a lambda" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(StepInto.line(5), StepOut.line(6))
      )
    }

    "should step in method that return this.type" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(12)(StepInto.line(4)),
        Breakpoint(15)(StepInto.line(4))
      )
    }

    "should step into default value method" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(StepInto.line(11), StepInto.line(6), StepInto.line(12)),
        Breakpoint(7)(
          StepInto.line(16),
          StepInto.line(7),
          StepInto.line(16),
          StepInto.line(7),
          StepInto.line(16)
        ),
        Breakpoint(8)(
          StepInto.line(18),
          StepInto.line(21),
          StepOut.line(18),
          StepOut.line(8),
          StepInto.method("B.<init>(String)")
        )
      )
    }

    "should match method on parameters and return type" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(16)(StepInto.line(8)),
        Breakpoint(17)(StepInto.line(4))
      )
    }

    "should match all kinds of types" - {
      // To determine if a method should be stepped into, the StepFilterProvider tries to find the Scala method
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(22)(StepInto.line(12)),
        Breakpoint(24)(StepInto.line(13)),
        Breakpoint(27)(StepInto.line(14)),
        Breakpoint(28)(StepInto.line(15)),
        Breakpoint(30)(StepInto.line(16)),
        Breakpoint(31)(StepInto.line(17)),
        Breakpoint(32)(
          StepInto.method("BoxesRunTime.boxToInteger(int)"),
          StepOut.line(32),
          StepInto.line(18)
        ),
        Breakpoint(33)(StepInto.line(19))
      )
    }

    "step into method with constant result type" - {
      if (isScala213) {
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
        assertInMainClass(source, "example.Main")(
          Breakpoint(7)(StepInto.line(4))
        )
      }
    }

    "step into method with alias result type" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(12)(
          StepInto.line(8)
        )
      )
    }

    "step into method with refined result type" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(16)(StepInto.line(8)),
        Breakpoint(17)(StepInto.line(12))
      )
    }

    "step into method with parametric result type" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(20)(StepInto.line(8)),
        Breakpoint(21)(StepInto.line(9))
      )
    }

    "step into method with external nested class" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(12)(StepInto.line(9))
      )
    }

    "step into method with external nested class" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(12)(StepInto.line(9))
      )
    }

    "should match Null and Nothing" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(9)(StepInto.line(5)),
        Breakpoint(10)(StepInto.line(4))
      )
    }

    "should catch ClassNotLoadedException" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(10)(StepInto.line(5))
      )
    }

    "should match on Array[T] whose erasure is Object" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(8)(StepInto.line(4))
      )
    }

    "should match on PolyType" - {
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
      assertInMainClass(source, "example.Main")(
        Breakpoint(13)(StepInto.line(4)),
        Breakpoint(14)(StepInto.line(4))
      )
    }

    "should step into trait initializer, if it is not empty" - {
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
           |    println(classOf[D])
           |    val d = new D
           |    d.a
           |    d.b
           |    d.b
           |    d.c
           |  }
           |}
           |
           |""".stripMargin
      val breakpoints = if (isScala3) {
        Seq(
          Breakpoint(20)(
            StepInto.method("Class.getDeclaredField(String)"),
            StepOut.line(20),
            StepInto.method("D.<init>()"),
            StepInto.method("Object.<init>()"),
            StepInto.method("D.<init>()"),
            StepInto.method("A.$init$(A)"),
            StepInto.method("D.<init>()"),
            StepInto.method("Statics.releaseFence()")
          ),
          Breakpoint(22)(StepInto.line(8)),
          Breakpoint(23)(StepInto.line(24), StepInto.line(12))
        )
      } else {
        Seq(
          Breakpoint(20)(
            StepInto.method("D.<init>()"),
            StepInto.method("Object.<init>()"),
            StepInto.method("D.<init>()"),
            StepInto.method("A.$init$(A)"),
            StepInto.method("A.$init$(A)"),
            StepInto.method("D.<init>()"),
            if (isScala213) StepInto.method("Statics.releaseFence()")
            else StepInto.line(20)
          ),
          Breakpoint(21)(StepInto.line(22), StepInto.line(8)),
          Breakpoint(23)(StepInto.line(24), StepInto.line(12))
        )
      }
      assertInMainClass(source, "example.Main")(breakpoints: _*)
    }

    "should match on vararg type" - {
      val source =
        """|package example
           |
           |case class A(as: String*)
           |
           |object Main {
           |  def main(xs: Array[String]): Unit = {
           |    val a = A("x", "y")
           |    println(a.as)
           |    val sc = StringContext("x", "y")
           |    println(sc.parts)
           |  }
           |}
           |""".stripMargin

      assertInMainClass(source, "example.Main")(
        Breakpoint(8)(StepInto.method("Predef$.println(Object)")),
        Breakpoint(10)(StepInto.method("Predef$.println(Object)"))
      )
    }

    "encode operator symbols" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    println(classOf[<>])
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

      assertInMainClass(source, "example.Main")(
        Breakpoint(6)(StepInto.method("$less$greater.<init>()")),
        Breakpoint(7)(StepInto.method("$less$greater.m()")),
        Breakpoint(8)(StepInto.method("Main$.$amp($less$greater)"))
      )
    }
  }
}
