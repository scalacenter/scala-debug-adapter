package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.FakeJdiMethod
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import com.sun.jdi.*
import munit.FunSuite
import tastyquery.Contexts.Context
import tastyquery.Flags
import tastyquery.Names.*
import tastyquery.Symbols.TermSymbol
import scala.jdk.OptionConverters.*

import java.{util => ju}

class Scala30UnpicklerTests extends Scala3UnpicklerTests(ScalaVersion.`3.0`)
class Scala31PlusUnpicklerTests extends Scala3UnpicklerTests(ScalaVersion.`3.1+`)

abstract class Scala3UnpicklerTests(val scalaVersion: ScalaVersion) extends FunSuite:
  test("mixin-forwarders") {
    val source =
      """|package example
         |
         |trait A {
         |  def m(): String = "A.m()"
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val a1 = new A {}
         |    val a2 = new A {
         |      override def m(): String = "g.m()"
         |    }
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
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    val javaSig = "java.lang.String m()"

    unpickler.assertFormat("example.A", javaSig, "A.m(): String")
    unpickler.assertNotFound("example.B", javaSig)
    unpickler.assertNotFound("example.C", javaSig)
    unpickler.assertFormat("example.D", javaSig, "D.m(): String")
    unpickler.assertNotFound("example.E", javaSig)
    unpickler.assertNotFound("example.F$", javaSig)
    unpickler.assertFormat("example.Main$G", javaSig, "Main.G.m(): String")
    unpickler.assertNotFound("example.Main$H", javaSig)
    unpickler.assertFailure("example.Main$$anon$1", javaSig)
    // TODO fix: we could find it by traversing the tree of `Main`
    unpickler.assertFailure("example.Main$$anon$2", javaSig)
  }

  test("local classes or local objects") {
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
         |  }
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFailure("example.Main$A$1", "void m()")
    unpickler.assertFailure("example.Main$B$2$", "void m()")
  }

  test("getters and setters") {
    val source =
      """|package example
         |
         |object Main {
         |  val x1 = "x1"
         |  private val x2 = "x2"
         |  var x3 = "x3"
         |  private var x4 = "x4"
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
         |}
         |
         |case class D(d1: String)
         |
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    def getter(field: String): String = s"java.lang.String $field()"
    def setter(field: String): String = s"java.lang.String $field$$eq(java.lang.String x$$1)"

    // When looking for a getter we find the symbol of the field
    unpickler.assertFind("example.Main$", getter("x1"))
    unpickler.assertFormat("example.Main$", getter("x1"), "Main.x1: String")

    unpickler.assertFind("example.Main$", getter("x2"))
    unpickler.assertFind("example.Main$", getter("x3"))
    unpickler.assertFind("example.Main$", getter("x4"))
    unpickler.assertFind("example.A", getter("a1"))
    unpickler.assertFind("example.A", getter("a2"))
    unpickler.assertFind("example.B", getter("b1"))
    unpickler.assertFind("example.B", getter("b2"))
    unpickler.assertFind("example.C", getter("c1"))
    unpickler.assertFind("example.C", getter("c2"))
    unpickler.assertFind("example.C", getter("c3"))
    unpickler.assertFind("example.D", getter("d1"))

    // there is no corresponding symbol in TASTy query
    // should we return the field symbol?
    unpickler.assertNotFound("example.Main$", setter("x3"))
    unpickler.assertNotFound("example.Main$", setter("x4"))
  }

  test("bridges") {
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
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    def javaSig(returnType: String): String = s"$returnType m()"

    unpickler.assertFormat("example.A", javaSig("java.lang.Object"), "A.m(): Object")
    unpickler.assertNotFound("example.B", javaSig("java.lang.Object"))
    unpickler.assertFormat("example.B", javaSig("java.lang.String"), "B.m(): String")
  }

  test("extension method of value classes") {
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
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.A$", "java.lang.String m$extension(java.lang.String $this)", "A.m(): String")
  }

  test("multi parameter lists") {
    val source =
      """|package example
         |
         |object Main {
         |  def m()(a: A): String = {
         |    a.toString
         |  }
         |}
         |
         |class A
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "java.lang.String m(example.A a)", "Main.m()(a: A): String")
  }

  test("lazy initializer") {
    val source =
      """|package example
         |
         |object A extends B {
         |  lazy val a = {
         |    "a".toString
         |  }
         |}
         |
         |trait B {
         |  lazy val b = {
         |    "b".toString
         |  }
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    unpickler.assertFormat("example.A$", "java.lang.String a()", "A.a: String")
    unpickler.assertNotFound("example.A$", "java.lang.String b()")
    unpickler.assertFormat("example.B", "java.lang.String b()", "B.b: String")
  }

  test("synthetic methods of case class") {
    val source =
      """|package example
         |
         |case class A(a: String)
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    unpickler.assertFormat("example.A", "java.lang.String toString()", "A.toString(): String")
    unpickler.assertFormat("example.A", "example.A copy(java.lang.String a)", "A.copy(a: String): A")
    unpickler.assertFormat("example.A", "int hashCode()", "A.hashCode(): Int")
    unpickler.assertFormat("example.A", "boolean equals(java.lang.Object x$0)", "A.equals(x$0: Any): Boolean")
    unpickler.assertFormat("example.A", "int productArity()", "A.productArity: Int")
    unpickler.assertFormat("example.A", "java.lang.String productPrefix()", "A.productPrefix: String")
    unpickler.assertFormat("example.A", "java.lang.Object productElement(int n)", "A.productElement(n: Int): Any")
    unpickler.assertNotFound("example.A", "scala.collection.Iterator productIterator()") // it is a bridge

    unpickler.assertFormat("example.A$", "example.A apply(java.lang.String a)", "A.apply(a: String): A")
    unpickler.assertFormat("example.A$", "example.A unapply(example.A x$1)", "A.unapply(x$1: A): A")
  }

  test("anonymous function") {
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
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    // TODO fix: it should find the symbol f by traversing the tree of object Main
    unpickler.assertNotFound("example.Main$", "int $anonfun$1(int x)")
  }

  test("this.type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): this.type = this
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.A", "example.A m()", "A.m(): A")
  }

  test("default values") {
    val source =
      """|package example
         |
         |case class A(a1: String = "", y: Int = 2) {
         |  def m(x: String = "", y: Int = 2): String = {
         |    x * 2
         |  }
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    // TODO fix: should be able to find a1, y, x and the other y by traversing the trees of m and <init>
    unpickler.assertNotFound("example.A", "java.lang.String m$default$1()")
    unpickler.assertNotFound("example.A", "int m$default$1()")
    unpickler.assertNotFound("example.A$", "java.lang.String $lessinit$greater$default$1()")
    unpickler.assertNotFound("example.A$", "int $lessinit$greater$default$2()")
  }

  test("matches on return types") {
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
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    unpickler.assertFormat("example.A", "int m(scala.collection.immutable.List xs)", "A.m(xs: List[Int]): Int")
    unpickler.assertNotFound("example.B", "int m(scala.collection.immutable.List xs)")
    unpickler.assertFormat(
      "example.B",
      "java.lang.String m(scala.collection.immutable.List xs)",
      "B.m(xs: List[String]): String"
    )
  }

  test("all kinds of types") {
    val source: String =
      """|package example
         |
         |class annot
         |  extends scala.annotation.Annotation
         |  with scala.annotation.StaticAnnotation
         |
         |trait A {
         |  class B
         |}
         |object Main extends A {
         |  def m(a : example.A): example.A = a
         |  def mbis(b: A#B): A#B = b
         |  def mbis(a: A)(b: a.B): a.B = b
         |  def m(a: this.type): this.type = a
         |  def mbis(a: A { def b: B }): A { def b: B } = a
         |  def m(x: String @annot): String @annot = x
         |  def m[T](x: T): T = x
         |  def mbis(a: Main.type): Main.type = a
         |  def m(x: => Int ): Int = 1
         |  def m(x : Int => Int): Int = 1
         |  def m(x : (Int,Int)) : Int = 1
         |  def mter(x: 1&1): 1|1 = 1
         |  def m(x: Int): Option[?] = Some(x)
         |  def m(a: A { type B })(b: a.type): b.B = new a.B
         |  val x: A = new A {}
         |  def m(a: x.type)(b: x.B): A = a
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)

    def assertFormat(javaSig: String, expected: String)(using munit.Location): Unit =
      unpickler.assertFormat("example.Main$", javaSig, expected)

    assertFormat("example.A m(example.A a)", "Main.m(a: A): A")
    assertFormat("example.A$B mbis(example.A$B b)", "Main.mbis(b: A.B): A.B")
    assertFormat("example.A$B mbis(example.A a, example.A$B b)", "Main.mbis(a: A)(b: a.B): a.B")
    assertFormat("example.Main$ m(example.Main$ a)", "Main.m(a: Main): Main")
    assertFormat("example.A mbis(example.A a)", "Main.mbis(a: A {...}): A {...}")
    assertFormat("java.lang.String m(java.lang.String x)", "Main.m(x: String): String")
    assertFormat("java.lang.Object m(java.lang.Object x)", "Main.m[T](x: T): T")
    assertFormat("example.Main$ mbis(example.Main$ a)", "Main.mbis(a: Main.type): Main.type")
    assertFormat("int m(scala.Function0 x)", "Main.m(x: => Int): Int")
    assertFormat("int m(scala.Function1 x)", "Main.m(x: Int => Int): Int")
    assertFormat("int m(scala.Tuple2 x)", "Main.m(x: (Int,Int)): Int")

    // TODO fix: should be m
    assertFormat("int mter(int x)", "Main.mter(x: 1 & 1): 1 | 1")
    assertFormat("scala.Option m(int x)", "Main.m(x: Int): Option[?]")
    assertFormat("example.A$B m(example.A a, example.A b)", "Main.m(a: A {...})(b: a.type): b.B")
    assertFormat("example.A m(example.A a, example.A$B b)", "Main.m(a: x.type)(b: x.B): A")
  }

  test("constant type") {
    val source =
      """|package example
         |
         |class A {
         |  def m1(x: "a"): 1 = 1
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.A", "int m1(java.lang.String x)", "A.m1(x: \"a\"): 1")
  }

  test("type aliases") {
    val source =
      """|package example
         |
         |class A
         |
         |object Main {
         |  type Foo = A
         |  type Bar = String
         |  def m(x: Foo): Bar  = x.toString
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "java.lang.String m(example.A m)", "Main.m(x: Foo): Bar")
  }

  test("refined types") {
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
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "example.B m1()", "Main.m1(): A & B {...}")
    unpickler.assertFormat("example.Main$", "java.lang.Object m2()", "Main.m2(): Object {...}")
  }

  test("type parameters") {
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
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.B", "example.A m1(example.A x)", "B.m1(x: X): X")
    unpickler.assertFormat("example.B", "example.A m2(example.A x)", "B.m2[T](x: T): T")
  }

  test("nested classes") {
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
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "scala.Enumeration$Value today()", "Main.today(): Enumeration.Value")
  }

  test("matches Null and Nothing") {
    val source =
      """|package example
         |
         |object Main {
         |  def m(xs: Array[Int]): Nothing = ???
         |  def m(xs: Array[String]): Null = null
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "scala.runtime.Nothing$ m(int[] xs)", "Main.m(xs: Array[Int]): Nothing")
    unpickler.assertFormat(
      "example.Main$",
      "scala.runtime.Null$ m(java.lang.String[] xs)",
      "Main.m(xs: Array[String]): Null"
    )
  }

  test("matches Array whose erasure is Object") {
    val source =
      """|package example
         |
         |object Main {
         |  def m[T](xs: Array[T]): Array[T] = xs
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat(
      "example.Main$",
      "java.lang.Object m(java.lang.Object xs)",
      "Main.m[T](xs: Array[T]): Array[T]"
    )
  }

  test("matches PolyType") {
    val source =
      """|package example
         |
         |class A[B[_]] {
         |  def m[T](x: B[T]): B[T] = x
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.A", "java.lang.Object m(java.lang.Object x)", "A.m[T](x: B[T]): B[T]")
  }

  test("trait initializers") {
    val source =
      """|package example
         |
         |trait A {
         |  val a: String = "a"
         |}
         |
         |class B extends A
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.A", "void $init$(example.A $this)", "A.<init>(): Unit")
    unpickler.assertFormat("example.B", "example.B <init>()", "B.<init>(): Unit")

  }

  test("vararg type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(as: String*): String = as.mkString(", ")
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    // TODO print String* instead of <repeated>[String]
    unpickler.assertFormat(
      "example.A",
      "java.lang.String m(scala.collection.immutable.Seq as)",
      "A.m(as: <repeated>[String]): String"
    )
  }

  test("encoded symbols") {
    val source =
      """|package example
         |
         |object Main {
         |  def &(x: <>): String = x.toString
         |}
         |
         |class <> {
         |  def m: <> = this
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "java.lang.String $amp(example.$less$greater x)", "Main.&(x: <>): String")
    unpickler.assertFormat("example.$less$greater", "example.$less$greater m()", "<>.m: <>")
  }

  test("local recursive method") {
    val source =
      """|package example
         |
         |object Main {
         |  def fac(x: Int): Int = {
         |    def rec(x: Int, acc: Int): Int = {
         |      if (x <= 0) acc
         |      else rec(x - 1, acc * x)
         |    }
         |    rec(x, 1)
         |  }
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    // TODO fix: find rec by traversing the tree of object Main
    unpickler.assertNotFound("example.Main$", "int rec$1(int x, int acc)")
  }

  test("local lazy initializer") {
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
         |  }
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    // TODO fix: find foo by traversing the tree of object Main
    unpickler.assertNotFound("example.Main$", "java.lang.String foo$lzyINIT1$1(scala.runtime.LazyRef foo$lzy1$1)")
    unpickler.assertNotFound("example.Main$", "java.lang.String foo$1(scala.runtime.LazyRef foo$lzy1$2)")
  }

  test("private methods made public") {
    val source =
      """|package example
         |
         |class Outer {
         |  private def foo: String = "foo"
         |  class Inner {
         |    def bar: String = {
         |      foo
         |    }
         |  }
         |}
         |
         |class A {
         |  def m: Int = A.m
         |}
         |
         |object A {
         |  private def m: Int = 1
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Outer", "java.lang.String example$Outer$$foo()", "Outer.foo: String")
    unpickler.assertFormat("example.A$", "int example$A$$$m()", "A.m: Int")
  }

  test("type lambda") {
    val source =
      """|package example
         |
         |trait Foo[F[_]]
         |
         |object Main:
         |  def foo : Foo[[X] =>> Either[X, Int]] = ???
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.Main$", "example.Foo foo()", "Main.foo: Foo[[X] =>> Either[X, Int]]")
  }

  test("package object") {
    val source =
      """|package object example {
         |  def foo: String = ???
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.package", "java.lang.String foo()", "example.foo: String")
  }

  test("top-level definition") {
    val source =
      """|package example
         |
         |def foo: String = ???
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example", scalaVersion)
    val unpickler = getUnpickler(debuggee)
    unpickler.assertFormat("example.example$package", "java.lang.String foo()", "example.foo: String")
  }

  private def getUnpickler(debuggee: Debuggee): Scala3Unpickler =
    val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))
    }
    val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
    new Scala3Unpickler(debuggeeClasspath, println, false)

  extension (unpickler: Scala3Unpickler)
    private def assertFind(declaringType: String, javaSig: String)(using munit.Location): Unit =
      val m = FakeJdiMethod(declaringType, javaSig)
      assert(unpickler.findSymbol(m).isDefined)

    private def assertNotFound(declaringType: String, javaSig: String)(using munit.Location): Unit =
      val m = FakeJdiMethod(declaringType, javaSig)
      assert(unpickler.findSymbol(m).isEmpty)

    private def assertFailure(declaringType: String, javaSig: String)(using munit.Location): Unit =
      val m = FakeJdiMethod(declaringType, javaSig)
      intercept[Exception](unpickler.findSymbol(m))

    private def assertFormat(declaringType: String, javaSig: String, expected: String)(using munit.Location): Unit =
      val m = FakeJdiMethod(declaringType, javaSig)
      assertEquals(unpickler.formatMethod(m).asScala, Some(expected))
