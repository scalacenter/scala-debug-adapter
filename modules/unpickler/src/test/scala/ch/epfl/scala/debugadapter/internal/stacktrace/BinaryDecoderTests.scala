package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import scala.util.Properties
import tastyquery.Exceptions.*

class Scala33BinaryDecoderTests extends BinaryDecoderTests(ScalaVersion.`3.3`)
class Scala34BinaryDecoderTests extends BinaryDecoderTests(ScalaVersion.`3.4`)

abstract class BinaryDecoderTests(scalaVersion: ScalaVersion) extends BinaryDecoderSuite:
  def isScala33 = scalaVersion.isScala33
  def isScala34 = scalaVersion.isScala34

  test("mixin and static forwarders") {
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
    val decoder = TestingDecoder(source, scalaVersion)
    val javaSig = "java.lang.String m()"
    val staticTraitAccessor = "java.lang.String m$(example.A $this)"

    decoder.assertDecode("example.A", javaSig, "A.m(): String")
    decoder.assertDecode("example.A", staticTraitAccessor, "A.m.<static forwarder>(): String", skip = true)
    decoder.assertDecode("example.B", javaSig, "B.m.<mixin forwarder>(): String", skip = true)
    decoder.assertDecode("example.C", javaSig, "C.m.<mixin forwarder>(): String", skip = true)
    decoder.assertDecode("example.D", javaSig, "D.m(): String")
    decoder.assertDecode("example.F$", javaSig, "F.m.<mixin forwarder>(): String", skip = true)
    decoder.assertDecode("example.F", javaSig, "F.m.<static forwarder>(): String", skip = true)
    decoder.assertDecode("example.Main$G", javaSig, "Main.G.m(): String")
    decoder.assertDecode("example.Main$H", javaSig, "Main.H.m.<mixin forwarder>(): String", skip = true)
    decoder.assertDecode(
      "example.Main$$anon$1",
      javaSig,
      "Main.main.<anon class>.m.<mixin forwarder>(): String",
      skip = true
    )
    decoder.assertDecode("example.Main$$anon$2", javaSig, "Main.main.<anon class>.m(): String")
  }

  test("local class, trait and object by parents") {
    val source =
      """|package example
         |object Main :
         |  class A
         |  def main(args: Array[String]): Unit = 
         |    trait D extends A
         |    class C extends D
         |    object F extends D
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$D$1", "Main.main.D")
    decoder.assertDecode("example.Main$C$1", "Main.main.C")
    decoder.assertDecode("example.Main$F$2$", "Main.main.F")
    decoder.assertDecode(
      "example.Main$",
      "example.Main$F$2$ F$1(scala.runtime.LazyRef F$lzy1$2)",
      "Main.main.F: F",
      skip = true
    )
    decoder.assertDecode(
      "example.Main$",
      "example.Main$F$2$ F$lzyINIT1$1(scala.runtime.LazyRef F$lzy1$1)",
      "Main.main.F.<lazy init>: F"
    )
  }

  test("local class and local method in a local class") {
    val source =
      """|package example
         |object Main {
         |  def m =
         |    class A // Main$A$1
         |    class Bar :
         |      def A = ()
         |      def m =
         |        class A  // Main$A$2
         |        def A() = () // Main.example$Main$Bar$1$$_$A$3()
         |        A()
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "void example$Main$Bar$1$$_$A$3()", "Main.m.….m.A(): Unit")
  }

  test("local methods with same name") {
    val source =
      """|package example
         |
         |class A {
         |  val x = "x"
         |  def m1: Unit = {
         |    val y = "y"
         |    def m: Unit = {
         |      def m(z: String): Unit =
         |        println(x + y + z)
         |      m("z")
         |    }
         |    m
         |  }
         |
         |  def m2: Unit = {
         |    def m(i: Int): Unit = println(i)
         |    m(1)
         |  }
         |
         |  def m3: Unit = {
         |    def m(i: Int): Unit = println(i)
         |    m(2)
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "void m$1(java.lang.String y$1)", "A.m1.m: Unit")
    decoder.assertDecode(
      "example.A",
      "void m$2(java.lang.String y$2, java.lang.String z)",
      "A.m1.m.m(z: String): Unit"
    )
    decoder.assertDecode("example.A", "void m$3(int i)", "A.m2.m(i: Int): Unit")
  }

  test("getters and setters") {
    val source =
      """|package example
         |
         |object Main {
         |  val x1 = "x1"
         |  var x2 = "x2"
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
         |class C(val c1: String) extends B with A {
         |  override val a1: String = "a1"
         |  override val a2: String = "a2"
         |}
         |
         |case class D(d1: String)
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    def getter(field: String): String = s"java.lang.String $field()"
    def setter(field: String, param: String = "x$1"): String = s"void ${field}_$$eq(java.lang.String $param)"

    decoder.assertDecode("example.Main$", getter("x1"), "Main.x1: String", skip = true)
    decoder.assertDecode("example.Main$", getter("x2"), "Main.x2: String", skip = true)
    decoder.assertDecode("example.Main$", setter("x2"), "Main.x2_=(String): Unit", skip = true)

    // static forwarders
    decoder.assertDecode("example.Main", getter("x1"), "Main.x1.<static forwarder>: String", skip = true)
    decoder.assertDecode("example.Main", getter("x2"), "Main.x2.<static forwarder>: String", skip = true)
    decoder.assertDecode(
      "example.Main",
      setter("x2", param = "arg0"),
      "Main.x2_=.<static forwarder>(String): Unit",
      skip = true
    )

    decoder.assertDecode("example.A", getter("a1"), "A.a1: String", skip = true)
    decoder.assertDecode("example.A", getter("a2"), "A.a2: String")
    decoder.assertDecode("example.B", getter("b1"), "B.b1: String", skip = true)
    decoder.assertDecode("example.B", getter("b2"), "B.b2: String", skip = true)
    decoder.assertDecode("example.C", getter("c1"), "C.c1: String", skip = true)
    decoder.assertDecode("example.D", getter("d1"), "D.d1: String", skip = true)
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
    val decoder = TestingDecoder(source, scalaVersion)

    def javaSig(returnType: String): String = s"java.lang.Object m()"

    decoder.assertDecode("example.A", "java.lang.Object m()", "A.m(): Object")
    decoder.assertDecode("example.B", "java.lang.Object m()", "B.m.<bridge>(): String", skip = true)
    decoder.assertDecode("example.B", "java.lang.String m()", "B.m(): String")
  }

  test("find outter field") {
    val source =
      """|package example
         |class A:
         |  private val x =2 
         |  class B[T]: 
         |    class C:
         |      private val y = x+2
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode(
      "example.A$B$C",
      "example.A$B example$A$B$C$$$outer()",
      "A.B.C.<outer>: B.this.type",
      skip = true
    )
  }

  test("using and implicit parameters") {
    val source =
      """|package example
         |object Main{
         |  def m1(using x : Int , y : Int) = x+y
         |  def m2(implicit x : Int) = x+1
         |  def m3(using String , Int): Unit = ()
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "int m1(int x, int y)", "Main.m1(using x: Int, y: Int): Int")
    decoder.assertDecode("example.Main$", "int m2(int x)", "Main.m2(implicit x: Int): Int")
    decoder.assertDecode(
      "example.Main$",
      "void m3(java.lang.String x$1, int x$2)",
      "Main.m3(using String, Int): Unit"
    )

    // static forwarders
    decoder.assertDecode(
      "example.Main",
      "int m1(int arg0, int arg1)",
      "Main.m1.<static forwarder>(using x: Int, y: Int): Int",
      skip = true
    )
    decoder.assertDecode(
      "example.Main",
      "int m2(int arg0)",
      "Main.m2.<static forwarder>(implicit x: Int): Int",
      skip = true
    )
    decoder.assertDecode(
      "example.Main",
      "void m3(java.lang.String arg0, int arg1)",
      "Main.m3.<static forwarder>(using String, Int): Unit",
      skip = true
    )

  }

  test("find local classes") {
    val source =
      """|package example
         |class A 
         |trait B         
         |object Main:
         |  def m() = 
         |    class C extends A,B 
         |    ()
         |    class E :
         |      class F 
         |    class G extends A
         |  def l () = 
         |    class C extends A 
         |    class G extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$C$1", "Main.m.C")
    decoder.assertDecode("example.Main$E$1$F", "Main.m.E.F")
    decoder.assertDecode("example.Main$G$1", "Main.m.G")
  }

  test("local class in signature") {
    val source =
      """|package example
         |object Main :
         |  class A :
         |    def m =
         |      class B :
         |        println("B")
         |        class C :
         |          def m =
         |            class D 
         |            def m(t : D) : D = 
         |              t
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "example.Main$D$1 m$1(example.Main$D$1 t)", "Main.A.….m.m(t: D): D")
    decoder.assertDecode("example.Main$A$B$1", "void <init>()", "Main.A.….B.<init>(): Unit")
  }

  test("operator-like names") {
    val source =
      """|package example 
         |class ++ :
         |  def m = 
         |    def ++ = 1
         |    class ++
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.$plus$plus", "int $plus$plus$1()", "++.m.++: Int")
    decoder.assertDecode("example.$plus$plus$$plus$plus$2", "++.m.++")
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
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A$", "java.lang.String m$extension(java.lang.String $this)", "A.m(): String")
    decoder.assertDecode(
      "example.A",
      "java.lang.String m$extension(java.lang.String arg0)",
      "A.m.<static forwarder>(): String",
      skip = true
    )
    decoder.assertDecode("example.A", "void <init>(java.lang.String x)", "A.<init>(x: String): Unit")
  }

  test("local method inside a value class") {
    val source =
      """|package example
         |
         |class A(val x: String) extends AnyVal {
         |  def m: String = {
         |    def m(t : String) : String = {
         |      t
         |    }
         |    m("")
         |  }
         |}
         |
         |object A {
         |  def m: String = {
         |    def m : String = "m"
         |    m
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A$", "java.lang.String m$2(java.lang.String t)", "A.m.m(t: String): String")
    decoder.assertDecode("example.A$", "java.lang.String m$1()", "A.m.m: String")
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "java.lang.String m(example.A a)", "Main.m()(a: A): String")
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

    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A$", "java.lang.String a()", "A.a: String", skip = true)
    decoder.assertDecode("example.A$", "java.lang.String b()", "A.b: String", skip = true)
    decoder.assertDecode("example.B", "java.lang.String b()", "B.b: String")
    decoder.assertDecode(
      "example.B",
      "java.lang.String b$(example.B $this)",
      "B.b.<static forwarder>: String",
      skip = true
    )

    // new in Scala 3.3.0
    decoder.assertDecode("example.A$", "java.lang.Object a$lzyINIT1()", "A.a.<lazy init>: String")
    decoder.assertDecode("example.A$", "java.lang.Object b$lzyINIT1()", "A.b.<lazy init>: String", skip = true)

    // static forwarders
    decoder.assertDecode("example.A", "java.lang.String a()", "A.a.<static forwarder>: String", skip = true)
    decoder.assertDecode("example.A", "java.lang.String b()", "A.b.<static forwarder>: String", skip = true)
  }

  test("synthetic methods of case class") {
    val source =
      """|package example
         |
         |case class A(a: String)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A", "java.lang.String toString()", "A.toString(): String", skip = true)
    decoder.assertDecode("example.A", "example.A copy(java.lang.String a)", "A.copy(a: String): A", skip = true)
    decoder.assertDecode("example.A", "int hashCode()", "A.hashCode(): Int", skip = true)
    decoder.assertDecode("example.A", "boolean equals(java.lang.Object x$0)", "A.equals(Any): Boolean", skip = true)
    decoder.assertDecode("example.A", "int productArity()", "A.productArity: Int", skip = true)
    decoder.assertDecode("example.A", "java.lang.String productPrefix()", "A.productPrefix: String", skip = true)
    decoder.assertDecode(
      "example.A",
      "java.lang.Object productElement(int n)",
      "A.productElement(n: Int): Any",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      "scala.collection.Iterator productIterator()",
      "A.productIterator.<mixin forwarder>: Iterator[Any]",
      skip = true
    )

    decoder.assertDecode("example.A$", "example.A apply(java.lang.String a)", "A.apply(a: String): A", skip = true)
    decoder.assertDecode("example.A$", "example.A unapply(example.A x$1)", "A.unapply(A): A", skip = true)
  }

  test("anonymous functions") {
    val source =
      """|package example
         |class A :
         |  class B : 
         |      def m =
         |        List(true).map(x => x.toString + 1)
         |        val f: Int => String = x => ""
         |  def m =
         |    List("").map(x => x + 1)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String m$$anonfun$1(boolean x)",
      "A.B.m.<anon fun>(x: Boolean): String"
    )
    decoder.assertDecode("example.A", "java.lang.String $anonfun$1(int x)", "A.B.m.<anon fun>(x: Int): String")
    decoder.assertDecode(
      "example.A",
      "java.lang.String m$$anonfun$2(java.lang.String x)",
      "A.m.<anon fun>(x: String): String"
    )
  }

  test("anonymous class") {
    val source =
      """|package example
         |class B :
         |  def n = 42
         |class A :
         |  def m(t: => Any): Int = 
         |    val b = new B {
         |      def m = ()
         |    }
         |    b.n
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A$$anon$1", "A.m.<anon class>")
  }

  test("this.type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): this.type = this
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "example.A m()", "A.m(): A.this.type")
  }

  test("inline def with anonymous class and method") {
    val source =
      """|package example
         |class A 
         |
         |inline def m: Unit = 
         |  val f = (x : Int) => x + 1
         |  val a = new A {
         |    println("")
         |  }
         |  if true then () else m
         |
         |class B : 
         |  def n = 
         |    m
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "int $anonfun$1(int x)", "example.m.<anon fun>(x: Int): Int")
    decoder.assertDecode("example.B$$anon$1", "example.m.<anon class>")
  }

  test("SAM and partial functions") {
    val source =
      """|package example
         |object Main {
         |    val foo: Ordering[String] = (x, y) => x.size - y.size
         |    val f: PartialFunction[(String), Int] = {
         |      case ("1") => 1
         |      case ("2") => 2
         |      case ("3") => 3
         |    }    
         |  }
         |
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$$anon$1", "Main.<anon Ordering>")
    decoder.assertDecode(
      "example.Main$$anon$1",
      "int compare(java.lang.String x, java.lang.String y)",
      "Main.<anon Ordering>.compare(x: String, y: String): Int"
    )
    decoder.assertDecode(
      "example.Main$$anon$1",
      "int compare(java.lang.Object x, java.lang.Object y)",
      "Main.<anon Ordering>.compare.<bridge>(x: String, y: String): Int",
      skip = true
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "Main.<anon PartialFunction>"
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "boolean isDefinedAt(java.lang.String x)",
      "Main.<anon PartialFunction>.isDefinedAt(x: String): Boolean"
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "boolean isDefinedAt(java.lang.Object x)",
      "Main.<anon PartialFunction>.isDefinedAt.<bridge>(x: String): Boolean",
      skip = true
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "java.lang.Object applyOrElse(java.lang.String x, scala.Function1 default)",
      "Main.<anon PartialFunction>.applyOrElse[A1, B1](x: A1, default: A1 => B1): B1"
    )
    decoder.assertDecode(
      "example.Main$$anon$2",
      "java.lang.Object applyOrElse(java.lang.Object x, scala.Function1 default)",
      "Main.<anon PartialFunction>.applyOrElse.<bridge>[A1, B1](x: A1, default: A1 => B1): B1",
      skip = true
    )
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
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A", "java.lang.String m$default$1()", "A.m.<default 1>: String")
    decoder.assertDecode("example.A", "int m$default$2()", "A.m.<default 2>: Int")
    decoder.assertDecode(
      "example.A$",
      "java.lang.String $lessinit$greater$default$1()",
      "A.<init>.<default 1>: String"
    )
    decoder.assertDecode("example.A$", "int $lessinit$greater$default$2()", "A.<init>.<default 2>: Int")
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
    val decoder = TestingDecoder(source, scalaVersion)

    decoder.assertDecode("example.A", "int m(scala.collection.immutable.List xs)", "A.m(xs: List[Int]): Int")
    decoder.assertDecode(
      "example.B",
      "int m(scala.collection.immutable.List xs)",
      "B.m.<mixin forwarder>(xs: List[Int]): Int",
      skip = true
    )
    decoder.assertDecode(
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
         |
         |case class !:[A, B](left: A, right: B)
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
         |  def m(x: => Int ): Int = 1
         |  def m(x : Int => Int): Int = 1
         |  def m(x : (Int,Int)) : Int = 1
         |  def m(x: 1 & 1): 1 | 1 = 1
         |  def m(x: Int): Option[?] = Some(x)
         |  def m(a: A { type B })(b: a.type): b.B = new a.B
         |  val x: A = new A {}
         |  def m(a: x.type)(b: x.B): A = a
         |  def m(t: Int !: Int) = 1
         |  def m() : [T] => List[T] => Option[T] = ???
         |  def mbis() : [T] => (List[T],List[T]) => Option[T] = ???
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)

    def assertDecode(javaSig: String, expected: String)(using munit.Location): Unit =
      decoder.assertDecode("example.Main$", javaSig, expected)

    assertDecode("example.A m(example.A a)", "Main.m(a: A): A")
    assertDecode("example.A$B mbis(example.A$B b)", "Main.mbis(b: A.B): A.B")
    assertDecode("example.A$B mbis(example.A a, example.A$B b)", "Main.mbis(a: A)(b: a.B): a.B")
    assertDecode("example.Main$ m(example.Main$ a)", "Main.m(a: Main.this.type): Main.this.type")
    assertDecode("example.A mbis(example.A a)", "Main.mbis(a: A {...}): A {...}")
    assertDecode("java.lang.String m(java.lang.String x)", "Main.m(x: String): String")
    assertDecode("java.lang.Object m(java.lang.Object x)", "Main.m[T](x: T): T")
    assertDecode("example.Main$ mbis(example.Main$ a)", "Main.mbis(a: Main.type): Main.type")
    assertDecode("int m(scala.Function0 x)", "Main.m(x: => Int): Int")
    assertDecode("int m(scala.Function1 x)", "Main.m(x: Int => Int): Int")
    assertDecode("int m(scala.Tuple2 x)", "Main.m(x: (Int, Int)): Int")
    assertDecode("int m(example.$bang$colon t)", "Main.m(t: Int !: Int): Int")
    assertDecode("int m(int x)", "Main.m(x: 1 & 1): 1 | 1")
    assertDecode("scala.Option m(int x)", "Main.m(x: Int): Option[?]")
    assertDecode("example.A$B m(example.A a, example.A b)", "Main.m(a: A {...})(b: a.type): b.B")
    assertDecode("example.A m(example.A a, example.A$B b)", "Main.m(a: x.type)(b: x.B): A")
    assertDecode("scala.Function1 m()", "Main.m(): [T] => List[T] => Option[T]")
    assertDecode("scala.Function2 mbis()", "Main.mbis(): [T] => (List[T], List[T]) => Option[T]")
  }

  test("constant type") {
    val source =
      """|package example
         |
         |class A {
         |  def m1(x: "a"): 1 = 1
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "int m1(java.lang.String x)", "A.m1(x: \"a\"): 1")
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "java.lang.String m(example.A x)", "Main.m(x: Foo): Bar")
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "example.B m1()", "Main.m1(): A & B {...}")
    decoder.assertDecode("example.Main$", "java.lang.Object m2()", "Main.m2(): Object {...}")
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "example.A m1(example.A x)", "B.m1(x: X): X")
    decoder.assertDecode("example.B", "example.A m2(example.A x)", "B.m2[T](x: T): T")
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "scala.Enumeration$Value today()", "Main.today(): Enumeration.Value")
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "scala.runtime.Nothing$ m(int[] xs)", "Main.m(xs: Array[Int]): Nothing")
    decoder.assertDecode(
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.Object m(java.lang.Object x)", "A.m[T](x: B[T]): B[T]")
  }

  test("constructors and trait constructors") {
    val source =
      """|package example
         |
         |trait A {
         |  val a: String = "a"
         |}
         |
         |class B extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "void $init$(example.A $this)", "A.<init>(): Unit")
    decoder.assertDecode("example.B", "void <init>()", "B.<init>(): Unit")
  }

  test("vararg type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(as: String*): String = as.mkString(", ")
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String m(scala.collection.immutable.Seq as)",
      "A.m(as: String*): String"
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

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "java.lang.String $amp(example.$less$greater x)", "Main.&(x: <>): String")
    decoder.assertDecode("example.$less$greater", "example.$less$greater m()", "<>.m: <>")
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

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "int rec$1(int x, int acc)", "Main.fac.rec(x: Int, acc: Int): Int")
  }

  test("local lazy initializer") {
    val source =
      """|package example
         |
         |class A {
         |  def m: Unit = {
         |    val x: String = "x"
         |    lazy val y = {
         |      x + "y"
         |    }
         |    println(y)
         |  }
         |}
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      if isScala33 then "java.lang.String y$1(java.lang.String x$2, scala.runtime.LazyRef y$lzy1$2)"
      else "java.lang.String y$1(scala.runtime.LazyRef y$lzy1$2, java.lang.String x$2)",
      "A.m.y: String",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "java.lang.String y$lzyINIT1$1(java.lang.String x$1, scala.runtime.LazyRef y$lzy1$1)"
      else "java.lang.String y$lzyINIT1$1(scala.runtime.LazyRef y$lzy1$1, java.lang.String x$1)",
      "A.m.y.<lazy init>: String"
    )
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
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Outer", "java.lang.String example$Outer$$foo()", "Outer.foo: String")
    decoder.assertDecode("example.A$", "int example$A$$$m()", "A.m: Int")
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

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$", "example.Foo foo()", "Main.foo: Foo[[X] =>> Either[X, Int]]")
  }

  test("local enum") {
    val source =
      """|package example
         |object Main :
         |  def m =
         |    enum A:
         |      case B
         |    ()
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.Main$A$1", "Main.m.A")
  }

  test("package object") {
    val source =
      """|package object example {
         |  def foo: String = ???
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.package$", "java.lang.String foo()", "example.foo: String")
    decoder.assertDecode(
      "example.package",
      "java.lang.String foo()",
      "example.foo.<static forwarder>: String",
      skip = true
    )
  }

  test("top-level definition") {
    val source =
      """|package example
         |
         |def foo: String = ???
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.decoder$package$", "java.lang.String foo()", "example.foo: String")
    decoder.assertDecode(
      "example.decoder$package",
      "java.lang.String foo()",
      "example.foo.<static forwarder>: String",
      skip = true
    )
  }

  test("i491") {
    val source =
      """|package example
         |
         |class A {
         |  val m: String = ""
         |  def m(x: String): String = ""
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.String m()", "A.m: String", skip = true)
    decoder.assertDecode("example.A", "java.lang.String m(java.lang.String x)", "A.m(x: String): String")
  }

  test("adapted anon fun") {
    val source =
      """|package example
         |
         |class A {
         |  def m(x: String): String = x.takeWhile(_ != '.')
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "boolean m$$anonfun$adapted$1(java.lang.Object _$1)",
      "A.m.<anon fun>.<adapted>(Char): Boolean",
      skip = true
    )
  }

  test("super args") {
    val source =
      """|package example
         |
         |class A1(x: => String)
         |class A2(x: Int)(y: String => String)
         |
         |object B1 extends A1("") {
         |  object B2 extends A2(5)(x => x)
         |}
         |
         |class C1 extends A1(
         |  ""
         |) {
         |  object C2 extends A2(5)(x => x)
         |  
         |  def m = {
         |    class C3 extends A1(
         |      ""
         |    ) {
         |      class C4 extends A2(5)({ x =>
         |          x + x
         |      })
         |    }
         |
         |    new A1("") {
         |      override def toString: String = ""
         |    }
         |  }
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B1$", "scala.Function0 B1$$superArg$1()", "B1.<init>.<super arg>: () => \"\"")
    decoder.assertDecode(
      "example.B1$",
      "scala.Function1 example$B1$$$B2$$superArg$1()",
      "B1.B2.<init>.<super arg>: String => String"
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function0 C1$superArg$1()",
      "C1.<init>.<super arg>: () => \"\""
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function1 example$C1$$C2$$superArg$1()",
      "C1.C2.<init>.<super arg>: String => String"
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function0 example$C1$$_$C3$superArg$1$1()",
      "C1.m.C3.<init>.<super arg>: () => \"\""
    )
    decoder.assertDecode(
      "example.C1",
      "scala.Function0 example$C1$$_$$anon$superArg$1$1()",
      "C1.m.<anon class>.<init>.<super arg>: () => \"\""
    )
    decoder.assertDecode(
      "example.C1$C3$1",
      "scala.Function1 example$C1$C3$1$$C4$superArg$1()",
      "C1.m.….C4.<init>.<super arg>: String => String"
    )
  }

  test("method returning a context function") {
    val source =
      """|package example
         |
         |class A:
         |  def m(x: Int): String ?=> String = ???
         |  def m(): (Int, String) ?=> Int = ???
         |  def m(x: String): Int ?=> String ?=> String = ???
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      if isScala33 then "java.lang.String m(int x, java.lang.String evidence$1)"
      else "java.lang.String m(int x, java.lang.String contextual$1)",
      "A.m(x: Int): String ?=> String"
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "int m(int evidence$2, java.lang.String evidence$3)"
      else "int m(int contextual$2, java.lang.String contextual$3)",
      "A.m(): (Int, String) ?=> Int"
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "java.lang.String m(java.lang.String x, int evidence$4, java.lang.String evidence$5)"
      else "java.lang.String m(java.lang.String x, int contextual$4, java.lang.String contextual$5)",
      "A.m(x: String): Int ?=> String ?=> String"
    )
    // TODO try in 3.3.2 or 3.3.3
    if isScala34 then
      val source =
        """|package example
           |
           |class A:
           |  def mbis: ? ?=> String = ???
           |""".stripMargin
      val decoder = TestingDecoder(source, scalaVersion)
      decoder.assertDecode("example.A", "java.lang.String mbis(java.lang.Object contextual$1)", "A.mbis: ? ?=> String")
  }

  test("trait param") {
    val source =
      """|package example
         |
         |trait A(val x: Int, var y: Int, z: Int)(using String)
         |
         |class B(x: Int)(using String) extends A(1, 2, 3)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    // todo fix: should be a BinaryTraitParamGetter
    decoder.assertDecode("example.B", "int x()", "B.x: Int", skip = true)
    decoder.assertDecode("example.B", "int y()", "B.y: Int", skip = true)
    decoder.assertDecode("example.B", "void y_$eq(int x$1)", "B.y_=(Int): Unit", skip = true)
    decoder.assertDecode("example.B", "int example$A$$z()", "B.z: Int", skip = true)
    decoder.assertDecode("example.B", "java.lang.String example$A$$x$4()", "B.x$4: String", skip = true)
  }

  test("lifted try") {
    assume(isScala33)
    val source =
      """|package example
         |
         |class A:
         |  println("" + (try "" catch case e: Exception => ""))
         |
         |  val x = "" + 
         |    (try "" catch case e: Exception => "")
         |
         |  def m1 = 
         |    val x = "" + (try "" catch case e: Exception => "")
         |    def m2 = 1 + (try 2 catch case e: Exception => 3)
         |    x * m2
         |
         |  inline def m3 = try "" catch case e: Exception => ""
         |
         |   def m4 = "" + m3
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.String liftedTree1$1()", "A.<try>: \"\" | \"\"")
    decoder.assertDecode("example.A", "java.lang.String liftedTree2$1()", "A.<try>: \"\" | \"\"")
    decoder.assertDecode("example.A", "java.lang.String liftedTree3$1()", "A.m1.<try>: \"\" | \"\"")
    decoder.assertDecode("example.A", "int liftedTree4$1()", "A.m1.m2.<try>: 2 | 3")
    decoder.assertDecode("example.A", "java.lang.String liftedTree5$1()", "A.m4.<try>: \"\" | \"\"")
  }

  test("by-name args") {
    val source =
      """|package example
         |
         |class A {
         |  def foo[T](x: => T): T = x
         |
         |  foo("Hello")
         |  
         |  def m =
         |    foo(1 + 1)
         |}
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.Object foo(scala.Function0 x)", "A.foo[T](x: => T): T")
    decoder.assertDecode("example.A", "java.lang.String $init$$$anonfun$1()", "A.<by-name arg>: String")
    decoder.assertDecode("example.A", "int m$$anonfun$1()", "A.m.<by-name arg>: Int")
  }

  test("inner object") {
    val source =
      """|package example
         |
         |trait A:
         |  object B
         |
         |object C extends A:
         |  object D
         |
         |class E extends A:
         |  object F
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "example.A$B$ B()", "A.B: B")
    decoder.assertDecode("example.A", "example.A$B$ B$(example.A $this)", "A.B.<static forwarder>: B", skip = true)
    decoder.assertDecode("example.C$", "example.A$B$ B()", "C.B: B", skip = true)
    decoder.assertDecode("example.E", "example.E$F$ F()", "E.F: F", skip = true)
    decoder.assertDecode("example.E", "example.A$B$ B()", "E.B: B", skip = true)
    decoder.assertDecode("example.C$", "java.lang.Object B$lzyINIT1()", "C.B.<lazy init>: B", skip = true)
    decoder.assertDecode("example.E", "java.lang.Object F$lzyINIT1()", "E.F.<lazy init>: F")
    decoder.assertDecode("example.E", "java.lang.Object B$lzyINIT2()", "E.B.<lazy init>: B", skip = true)
  }

  test("static forwarder") {
    val source =
      """|package example
         |
         |class A[T] {
         |  def foo(x: T): String = "foo"
         |}
         |
         |object B extends A[String]
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "java.lang.String foo(java.lang.Object arg0)",
      "B.foo.<static forwarder>(x: String): String",
      skip = true
    )
  }

  test("param forwarders") {
    val source =
      """|package example
         |
         |class A[T](val foo: T)
         |
         |class B(foo: String) extends A(foo)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "java.lang.String foo$accessor()", "B.foo: String", skip = true)
  }

  test("trait setters") {
    val source =
      """|package example
         |
         |trait A:
         |  private val foo = "foo"
         |
         |class B extends A
         |
         |object C extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "void example$A$_setter_$example$A$$foo_$eq(java.lang.String x$0)",
      "B.foo.<setter>(String): Unit",
      skip = true
    )
    decoder.assertDecode(
      "example.C$",
      "void example$A$_setter_$example$A$$foo_$eq(java.lang.String x$0)",
      "C.foo.<setter>(String): Unit",
      skip = true
    )
    decoder.assertDecode(
      "example.C",
      "void example$A$_setter_$example$A$$foo_$eq(java.lang.String arg0)",
      "C.foo.<setter>.<static forwarder>(String): Unit",
      skip = true
    )
  }

  test("super accessors") {
    val source =
      """|package example
         |
         |class A[T]:
         |  def foo(x: T): String = "foo"
         |
         |trait B[T] extends A[T]:
         |  override def foo(x: T): String = super.foo(x) + "bar"
         |
         |class C extends B[String]
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "java.lang.String example$B$$super$foo(java.lang.Object x)",
      "B.foo.<super>(x: T): String",
      skip = true
    )
    decoder.assertDecode(
      "example.C",
      "java.lang.String example$B$$super$foo(java.lang.String x)",
      "C.foo.<super>(x: String): String",
      skip = true
    )
    decoder.assertDecode(
      "example.C",
      "java.lang.String example$B$$super$foo(java.lang.Object x)",
      "C.foo.<super>.<bridge>(x: String): String",
      skip = true
    )
  }

  test("java arg bridges") {
    val javaSource =
      """|package example;
         |
         |class A {
         |  public String m(Object... args) {
         |    return "";
         |  }
         |}
         |""".stripMargin
    val source =
      """|package example
         |
         |class B extends A:
         |  override def m(args: Any*): String = super.m(args)
         |
         |  @scala.annotation.varargs
         |  def m(args: String*): Int = args.size
         |""".stripMargin
    val javaModule = TestingDebuggee.fromJavaSource(javaSource, "example", scalaVersion).mainModule
    val decoder = TestingDecoder(source, scalaVersion, Seq(javaModule))
    decoder.assertDecode(
      "example.B",
      "java.lang.String m(java.lang.Object[] args)",
      "B.m.<bridge>(args: Any*): String",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String m(scala.collection.immutable.Seq args)",
      "B.m(args: Any*): String"
    )
    decoder.assertDecode(
      "example.B",
      "int m(java.lang.String[] args)",
      "B.m.<bridge>(args: String*): Int",
      skip = true
    )
    decoder.assertDecode("example.B", "int m(scala.collection.immutable.Seq args)", "B.m(args: String*): Int")
  }

  test("specialized methods") {
    val source =
      """|package example
         |
         |class A extends (Double => Boolean):
         |  def apply(x: Double): Boolean = x > 0
         |
         |object B extends A
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "boolean apply(double x)", "A.apply(x: Double): Boolean")
    decoder.assertDecode(
      "example.A",
      "java.lang.Object apply(java.lang.Object v1)",
      "A.apply.<bridge>(x: Double): Boolean",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      "boolean apply$mcZD$sp(double x)",
      "A.apply.<specialized>(x: Double): Boolean",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      "int apply$mcII$sp(int x$0)",
      "A.apply.<specialized>(x: Double): Boolean",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "boolean apply(double arg0)",
      "B.apply.<static forwarder>(x: Double): Boolean",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "boolean apply$mcZD$sp(double arg0)",
      "B.apply.<specialized>.<static forwarder>(x: Double): Boolean",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "int apply$mcII$sp(int arg0)",
      "B.apply.<specialized>.<static forwarder>(x: Double): Boolean",
      skip = true
    )
  }

  test("by-name arg proxy") {
    val source =
      """|package example
         |
         |trait A:
         |  def m[T](x: => T): T
         |
         |class B:
         |  def m(s: String): String =
         |    B.m(s * 2)
         |
         |object B extends A:
         |  inline override def m[T](x: => T): T = x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "java.lang.String x$proxy2$1(java.lang.String s$1)", "B.m.<by-name arg>: String")
    decoder.assertDecode("example.B$", "java.lang.Object x$proxy1$1(scala.Function0 x$1)", "B.m.<by-name arg>: T")
  }

  test("inline accessor") {
    val source =
      """|package example
         |
         |trait A:
         |  class AA:
         |    private[A] var x: String = "foo"
         |  inline def m(aa: AA): Unit = if aa.x == "foo" then aa.x = "bar"
         |
         |class B extends A
         |
         |object B:
         |  private var y: String = "foo"
         |  inline def m: Unit = if y == "foo" then y = "bar"
         |
         |class C(x: String) extends AnyVal:
         |  inline def m: String = x + x
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String inline$x$i2(example.A$AA x$0)",
      "A.<inline A.AA.x>: String",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      "java.lang.String inline$x$i2$(example.A $this, example.A$AA x$0)",
      "A.<inline A.AA.x>.<static forwarder>: String",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      "void inline$x_$eq$i2(example.A$AA x$0, java.lang.String x$0)",
      "A.<inline A.AA.x_=>(String): Unit",
      skip = true
    )
    decoder.assertDecode(
      "example.A",
      "void inline$x_$eq$i2$(example.A $this, example.A$AA x$0, java.lang.String x$0)",
      "A.<inline A.AA.x_=>.<static forwarder>(String): Unit",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String inline$x$i2(example.A$AA x$0)",
      "B.<inline A.AA.x>.<mixin forwarder>: String",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "void inline$x_$eq$i2(example.A$AA x$0, java.lang.String x$0)",
      "B.<inline A.AA.x_=>.<mixin forwarder>(String): Unit",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "java.lang.String inline$y()",
      "B.<inline B.y>.<static forwarder>: String",
      skip = true
    )
    decoder.assertDecode(
      "example.B",
      "void inline$y_$eq(java.lang.String arg0)",
      "B.<inline B.y_=>.<static forwarder>(String): Unit",
      skip = true
    )
    decoder.assertDecode(
      "example.C$",
      "java.lang.String inline$x$extension(java.lang.String $this)",
      "C.<inline C.x>: String",
      skip = true
    )
    decoder.assertDecode(
      "example.C",
      "java.lang.String inline$x$extension(java.lang.String arg0)",
      "C.<inline C.x>.<static forwarder>: String",
      skip = true
    )
  }

  test("deserializeLambda") {
    val source =
      """|package example
         |
         |object A:
         |  val x: String => String = identity
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A$",
      "java.lang.Object $deserializeLambda$(java.lang.invoke.SerializedLambda arg0)",
      "A.$deserializeLambda$(arg0: SerializedLambda): Object"
    )
  }

  test("java.lang.Enum constructor") {
    val source =
      """|package example
         |
         |enum A(x: String) extends java.lang.Enum[A]:
         |  case B extends A("b")
         |  case C extends A("c")
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "void <init>(java.lang.String x, java.lang.String _$name, int _$ordinal)",
      "A.<init>(x: String): Unit"
    )
  }

  test("anon lazy inits") {
    val source =
      """|package example
         |
         |class A:
         |  lazy val (x, y) = m
         |  def m =
         |    lazy val (x, y) = ("x", "y")
         |    (x, y)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "java.lang.Object $1$$lzyINIT1()", "A.<anon>.<lazy init>: (String, String)")
    decoder.assertDecode("example.A", "scala.Tuple2 $1$()", "A.<anon>: (String, String)", skip = true)
    decoder.assertDecode(
      "example.A",
      "scala.Tuple2 $2$$lzyINIT1$1(scala.runtime.LazyRef $2$$lzy1$1)",
      "A.m.<anon>.<lazy init>: (String, String)"
    )
    decoder.assertDecode(
      "example.A",
      "scala.Tuple2 $2$$1(scala.runtime.LazyRef $2$$lzy1$2)",
      "A.m.<anon>: (String, String)",
      skip = true
    )
  }

  test("trait local static forwarder") {
    val source =
      """|package example
         |
         |trait A:
         |  val x: String
         |  private def m1 =
         |    class B:
         |      def m2 = m3
         |    def m3: String = x + x
         |    () 
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      "java.lang.String example$A$$_$m3$1$(example.A $this)",
      "A.m1.m3.<static forwarder>: String",
      skip = true
    )
  }

  test("reduce ambiguity of anon funs by finding clashing methods") {
    val source =
      """|package example
         |
         |class A:
         |  def m =
         |    for {
         |      (tag, formatter) <- scala.collection.immutable.ListMap.empty[String, String]
         |      boundss <- Some(List.empty[(String, String)])
         |      texts = List.empty[String]
         |      formatted <- Some("")
         |    } yield formatted
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.A",
      if isScala33 then "scala.Option m$$anonfun$2(scala.Tuple2 x$1)"
      else "scala.Option m$$anonfun$1(scala.Tuple2 x$1)",
      "A.m.<anon fun>((String, String)): Option[String]"
    )
    decoder.assertDecode(
      "example.A",
      if isScala33 then "scala.Option m$$anonfun$2$$anonfun$2(scala.Tuple2 x$1)"
      else "scala.Option m$$anonfun$1$$anonfun$2(scala.Tuple2 x$1)",
      "A.m.<anon fun>.<anon fun>((List[(String, String)], List[String])): Option[String]"
    )
  }

  test("inline defs") {
    val source =
      """|package example
         |
         |class Logger:
         |  def m1(x: String): String = ???
         |
         |  inline def m2(f: String => String): String => String =
         |    x => m1(f(x))
         |
         |  inline def trace1(a: String)(inline f: String => String): Unit =
         |    println(f(a))
         |
         |  inline def rec(b: Boolean)(inline f: String => String): String =
         |    inline if b then f(rec(false)(f))
         |    else f("")
         |
         |class Test:
         |  def test(name: String): Unit =
         |    val logger = new Logger
         |    val xs = List.empty[String]
         |    val f = logger.m2(x => xs.map(y => x + y).mkString)
         |    logger.trace1(name)(x => xs.map(y => x + y).mkString)
         |    logger.trace1(name + name)(x => xs.map(y => x + y).mkString)
         |    logger.rec(true)(x => xs.map(y => x + y).mkString)
         |""".stripMargin

    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.Test",
      "java.lang.String $anonfun$1(scala.collection.immutable.List xs$1, java.lang.String x)",
      "Test.test.<anon fun>(x: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      if isScala33 then
        "java.lang.String $anonfun$2(scala.Function1 f$proxy1$1, example.Logger Logger_this$1, java.lang.String x)"
      else "java.lang.String $anonfun$2(example.Logger Logger_this$1, scala.Function1 f$proxy1$1, java.lang.String x)",
      "Logger.m2.<anon fun>(x: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$1(java.lang.String name$1, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$2(java.lang.String a$proxy1$1, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$3(java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String test$$anonfun$4(java.lang.String x$2, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
    decoder.assertDecode(
      "example.Test",
      "java.lang.String $anonfun$1$$anonfun$1(java.lang.String x$1, java.lang.String y)",
      "Test.test.<anon fun>.<anon fun>(y: String): String"
    )
  }

  test("tastyquery#395"):
    assume(!isJava8)
    val decoder = initDecoder("de.sciss", "desktop-core_3", "0.11.4")
    decoder.assertDecode(
      "de.sciss.desktop.impl.LogPaneImpl$textPane$",
      "boolean apply$mcZD$sp(double x$0)",
      "LogPaneImpl.textPane.apply.<specialized>(str: String): Unit",
      skip = true
    )

  test("tasty-query#397 and tasty-query#413"):
    val decoder = initDecoder("com.github.xuwei-k", "httpz_3", "0.8.0")
    decoder.assertDecode("httpz.package$$anon$1", "httpz.ActionZipAp.<anon class>")
    decoder.assertDecode("httpz.InterpretersTemplate$$anon$5", "InterpretersTemplate.times.….apply.<anon class>")
    decoder.assertDecode(
      "httpz.package$",
      "java.lang.Object httpz$package$$anon$1$$_$ap$$anonfun$1(scala.Function1 _$2, java.lang.Object _$3)",
      "httpz.ActionZipAp.….ap.<anon fun>(A => B, A): B"
    )
    decoder.assertDecode(
      "httpz.Response",
      "scalaz.Equal responseEqual(scalaz.Equal arg0)",
      "Response.responseEqual.<static forwarder>[A](implicit Equal[A]): Equal[Response[A]]",
      skip = true
    )
    decoder.assertDecode(
      "httpz.Core",
      "scalaz.$bslash$div jsonResponse$$anonfun$3$$anonfun$1(argonaut.DecodeJson A$2, httpz.Request request$1, httpz.Response json)",
      "Core.jsonResponse.<anon fun>.<anon fun>(json: Response[Json]): \\/[Error, Response[A]]"
    )

  test("tasty-query#398"):
    val decoder = initDecoder("io.github.ashwinbhaskar", "sight-client_3", "0.1.2")
    decoder.assertDecode(
      "sight.client.SightClientImpl",
      "java.lang.String b$1(scala.Tuple2 x$1$2)",
      "SightClientImpl.constructPayload.….<anon fun>.b: String"
    )

  test("tasty-query#401".ignore) {
    val source =
      """|package example
         |
         |type Rec[A <: Tuple] <: Tuple = A match
         |  case hd *: tl => hd *: Rec[tl]
         |  case EmptyTuple => EmptyTuple
         |
         |object Rec:
         |  inline def rec[A <: Tuple](a: A): Rec[A] = 
         |    inline a match
         |      case b: (hd *: tl) => b.head *: rec(b.tail)
         |      case _: EmptyTuple => EmptyTuple
         |
         |trait Codec[A]:
         |  def map[B](x: A => B): Codec[B]
         |
         |object Test:
         |  inline def rec[A <: Tuple](c: Codec[A]): Codec[Rec[A]] =
         |    c.map(x => Rec.rec(x))
         |  def foo(c: Codec[(Int, Int)]): Codec[(Int, Int)] =
         |    rec(c)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.Test$",
      "scala.Product foo$$anonfun$1(scala.Tuple2 x)",
      "Foo.drop1[A](a: A): DropUnits[A]"
    )
  }

  test("tasty-query#402"):
    val decoder = initDecoder("com.softwaremill.sttp.client3", "opentelemetry_3", "3.6.1")
    decoder.assertDecode(
      "sttp.client3.opentelemetry.OpenTelemetryTracingBackend",
      "java.lang.Object send$$anonfun$2$$anonfun$1$$anonfun$1(sttp.client3.RequestT request$5, scala.collection.mutable.Map carrier$5)",
      "OpenTelemetryTracingBackend.send.<anon fun>.<by-name arg>: F[Response[T]]"
    )

  test("tasty-query#403") {
    val source =
      """|package example
         |
         |trait A[T]:
         |  opaque type Type <: T = T
         |
         |class B extends A[String]:
         |  def m(x: Type): Unit = ???
         |
         |class Value[T](v: T) extends AnyVal
         |
         |class C[T <: Int]:
         |  def m(x: Value[T]): Unit = ???
         |  
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.B", "void m(java.lang.String x)", "B.m(x: Type): Unit")
    decoder.assertDecode("example.C", "void m(java.lang.Integer x)", "C.m(x: Value[T]): Unit")
  }

  test("tasty-query#407") {
    val source =
      """|package example
         |
         |import java.util.function.Consumer
         |
         |class A:
         |  def m(f: String => Unit): Consumer[String] =
         |    new Consumer[String]():
         |      def accept(line: String): Unit = f(line)
         |  
         |  def test: Consumer[String] = m(println)
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.A", "void test$$anonfun$1(java.lang.String x)", "A.test.<anon fun>(x: String): Unit")
  }

  test("scala3-compiler:3.3.1"):
    val decoder = initDecoder("org.scala-lang", "scala3-compiler_3", "3.3.1")
    decoder.assertDecode(
      "scala.quoted.runtime.impl.QuotesImpl",
      "boolean scala$quoted$runtime$impl$QuotesImpl$$inline$xCheckMacro()",
      "QuotesImpl.<inline QuotesImpl.xCheckMacro>: Boolean",
      skip = true
    )
    decoder.assertDecode(
      "dotty.tools.dotc.printing.RefinedPrinter",
      "void dotty$tools$dotc$printing$RefinedPrinter$$inline$myCtx_$eq(dotty.tools.dotc.core.Contexts$Context x$0)",
      "RefinedPrinter.<inline RefinedPrinter.myCtx_=>(Contexts.Context): Unit",
      skip = true
    )
    decoder.assertDecode(
      "dotty.tools.dotc.transform.sjs.PrepJSInterop$OwnerKind",
      "int inline$baseKinds$extension(int arg0)",
      "PrepJSInterop.OwnerKind.<inline PrepJSInterop.OwnerKind.baseKinds>.<static forwarder>: Int",
      skip = true
    )
    decoder.assertDecode(
      "org.scalajs.ir.Trees$OptimizerHints",
      "boolean inline$extension(int arg0)",
      "Trees.OptimizerHints.inline.<static forwarder>: Boolean",
      skip = true
    )
    decoder.assertDecode(
      "dotty.tools.package",
      "java.lang.Object unreachable$default$1()",
      "tools.unreachable.<default 1>.<static forwarder>: Any",
      skip = true
    )
    // decoder.assertDecode(
    //   "dotty.tools.dotc.printing.Formatting$StringFormatter",
    //   "java.lang.String assemble$$anonfun$1(java.lang.String str)",
    //   ""
    // )
    decoder.assertDecode(
      "dotty.tools.dotc.core.tasty.TreeUnpickler",
      "dotty.tools.dotc.ast.Trees$Tree dotty$tools$dotc$core$tasty$TreeUnpickler$TreeReader$$_$_$$anonfun$18(dotty.tools.dotc.core.Contexts$Context x$1$19, dotty.tools.dotc.core.tasty.TreeUnpickler$TreeReader $this$tailLocal1$1)",
      "TreeUnpickler.readTpt.<static forwarder>()(using Contexts.Context): tpd.Tree",
      skip = true
    )

  test("tasty-query#412"):
    val decoder = initDecoder("dev.zio", "zio-interop-cats_3", "23.1.0.0")(using ThrowOrWarn.ignore)
    decoder.assertDecode("zio.BuildInfoInteropCats", "BuildInfoInteropCats")
    decoder.assertDecode("zio.interop.ZioMonadErrorE$$anon$4", "ZioMonadErrorE.adaptError.<anon PartialFunction>")

  test("tasty-query#414"):
    val decoder = initDecoder("io.github.dieproht", "matr-dflt-data_3", "0.0.3")
    decoder.assertDecode(
      "matr.dflt.DefaultMatrixFactory$$anon$1",
      "DefaultMatrixFactory.defaultMatrixFactory.builder.<anon class>"
    )

  test("tasty-query#415"):
    val decoder = initDecoder("com.github.mkroli", "dns4s-fs2_3", "0.21.0")
    decoder.assertDecode("com.github.mkroli.dns4s.fs2.DnsClientOps", "DnsClientOps")

  test("bug: Type.of creates capture".ignore):
    val source =
      """|package example
         |
         |import scala.quoted.*
         |
         |trait A[T]
         |
         |class B(using q: Quotes):
         |  import q.reflect.*
         |
         |  private def m[T](using tpe: Type[T]): String => TypeRepr =
         |    (x: String) => TypeRepr.of[A[T]]
         |
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode(
      "example.B",
      "java.lang.Object m$$anonfun$1(scala.quoted.Type tpe$1, java.lang.String x)",
      "B.m.<anon fun>(x: String): q.reflect.TypeRepr"
    )

  test("bug: not an outer".ignore):
    val decoder = initDecoder("com.disneystreaming", "weaver-monix-core_3", "0.6.15")(using ThrowOrWarn.ignore)
    decoder.assertDecode(
      "weaver.monixcompat.PureTaskSuite",
      "weaver.SourceLocation$ weaver$SourceLocationMacro$Here$$$outer()",
      ""
    )

  test("bug: $ as a name".ignore) {
    val source =
      """|package example
         |
         |class $:
         |  def $: String = ???
         |
         |object $
         |""".stripMargin
    val decoder = TestingDecoder(source, scalaVersion)
    decoder.assertDecode("example.$", "java.lang.String $()", "$.$: String")
    decoder.assertDecode("example.$$", "$")
  }

  test("tasty-query#423"):
    val decoder = initDecoder("com.typesafe.akka", "akka-stream_3", "2.8.5")
    decoder.assertDecode("akka.stream.scaladsl.FlowOps$passedEnd$2$", "FlowOps.zipAllFlow.passedEnd")

  test("tasty-query#424"):
    val decoder = initDecoder("edu.gemini", "lucuma-itc-core_3", "0.10.0")
    decoder.assertDecode("lucuma.itc.ItcImpl", "ItcImpl")

  test("specialized class"):
    val decoder = initDecoder("org.scala-lang", "scala-library", "2.13.12")
    decoder.assertDecode("scala.runtime.java8.JFunction1$mcII$sp", "JFunction1$mcII$sp")

  test("local class in value class"):
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
         |""".stripMargin
    // tasty-query#428
    val decoder = TestingDecoder(source, scalaVersion)(using ThrowOrWarn.ignore)
    decoder.assertDecode("example.A$B$1", "A.m.B")
