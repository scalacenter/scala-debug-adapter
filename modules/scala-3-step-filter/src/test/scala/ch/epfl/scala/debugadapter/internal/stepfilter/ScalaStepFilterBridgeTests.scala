package ch.epfl.scala.debugadapter.internal.stepfilter

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

import java.{util => ju}

class Scala30StepFilterBridgeTests extends ScalaStepFilterBridgeTests(ScalaVersion.`3.0`)
class Scala31PlusStepFilterBridgeTests extends ScalaStepFilterBridgeTests(ScalaVersion.`3.1+`)

abstract class ScalaStepFilterBridgeTests(scalaVersion: ScalaVersion) extends FunSuite:
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
    val stepFilter = getStepFilter(debuggee)

    def findM(declaringType: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod(declaringType, "m")()("java.lang.String"))

    assert(findM("example.A").isDefined)
    assert(findM("example.B").isEmpty)
    assert(findM("example.C").isEmpty)
    assert(findM("example.D").isDefined)
    assert(findM("example.E").isEmpty)
    assert(findM("example.F$").isEmpty)
    assert(findM("example.Main$G").isDefined)
    assert(findM("example.Main$H").isEmpty)
    intercept[Exception](findM("example.Main$$anon$1"))
    // TODO fix: we could find it by traversing the tree of `Main`
    intercept[Exception](findM("example.Main$$anon$2"))
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
    val stepFilter = getStepFilter(debuggee)
    intercept[Exception](stepFilter.findSymbol(FakeJdiMethod("example.Main$A$1", "m")()("void")))
    intercept[Exception](stepFilter.findSymbol(FakeJdiMethod("example.Main$B$2$", "m")()("void")))
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
    val stepFilter = getStepFilter(debuggee)

    def findGetter(declaringType: String, field: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod(declaringType, field)()("java.lang.String"))

    def findSetter(declaringType: String, field: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod(declaringType, s"$field$eq")("x$1" -> "java.lang.String")("java.lang.String"))

    // When looking for a getter we find the symbol of the field
    assert(findGetter("example.Main$", "x1").isDefined)
    assert(findGetter("example.Main$", "x2").isDefined)
    assert(findGetter("example.Main$", "x3").isDefined)
    assert(findGetter("example.Main$", "x4").isDefined)
    assert(findGetter("example.A", "a1").isDefined)
    assert(findGetter("example.A", "a2").isDefined)
    assert(findGetter("example.B", "b1").isDefined)
    assert(findGetter("example.B", "b2").isDefined)
    assert(findGetter("example.C", "c1").isDefined)
    assert(findGetter("example.C", "c2").isDefined)
    assert(findGetter("example.C", "c3").isDefined)
    assert(findGetter("example.D", "d1").isDefined)

    // there is no corresponding symbol in TASTy query
    // should we return the field symbol?
    assert(findSetter("example.Main$", "x3").isEmpty)
    assert(findSetter("example.Main$", "x4").isEmpty)
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
    val stepFilter = getStepFilter(debuggee)

    def findM(declaringType: String, returnType: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod(declaringType, "m")()(returnType))

    assert(findM("example.A", "java.lang.Object").isDefined)
    assert(findM("example.B", "java.lang.Object").isEmpty)
    assert(findM("example.B", "java.lang.String").isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val extensionMethod =
      FakeJdiMethod("example.A$", "m$extension")("$this" -> "java.lang.String")("java.lang.String")
    assert(stepFilter.findSymbol(extensionMethod).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val method = FakeJdiMethod("example.Main$", "m")("a" -> "example.A")("java.lang.String")
    assert(stepFilter.findSymbol(method).isDefined)
  }

  test("should step into lazy initializer") {
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
    val stepFilter = getStepFilter(debuggee)
    def findLazyVal(declaringType: String, name: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod(declaringType, name)()("java.lang.String"))
    assert(findLazyVal("example.A$", "a").isDefined)
    assert(findLazyVal("example.A$", "b").isEmpty)
    assert(findLazyVal("example.B", "b").isDefined)
  }

  test("synthetic method of case class") {
    val source =
      """|package example
         |
         |case class A(a: String)
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)
    val toString = FakeJdiMethod("example.A", "toString")()("java.lang.String")
    val copy = FakeJdiMethod("example.A", "copy")("a" -> "java.lang.String")("example.A")
    val hashCode = FakeJdiMethod("example.A", "hashCode")()("int")
    val equals = FakeJdiMethod("example.A", "equals")("x$0" -> "java.lang.Object")("boolean")
    val productArity = FakeJdiMethod("example.A", "productArity")()("int")
    val productPrefix = FakeJdiMethod("example.A", "productPrefix")()("java.lang.String")
    val productElement = FakeJdiMethod("example.A", "productElement")("n" -> "int")("java.lang.Object")
    val productIterator = FakeJdiMethod("example.A", "productIterator")()("scala.collection.Iterator")
    val apply = FakeJdiMethod("example.A$", "apply")("a" -> "java.lang.String")("example.A")
    val unapply = FakeJdiMethod("example.A$", "unapply")("x$1" -> "example.A")("example.A")
    assert(stepFilter.findSymbol(toString).isDefined)
    assert(stepFilter.findSymbol(copy).isDefined)
    assert(stepFilter.findSymbol(hashCode).isDefined)
    assert(stepFilter.findSymbol(equals).isDefined)
    assert(stepFilter.findSymbol(productArity).isDefined)
    assert(stepFilter.findSymbol(productPrefix).isDefined)
    assert(stepFilter.findSymbol(productElement).isDefined)
    assert(stepFilter.findSymbol(productIterator).isEmpty) // it is a bridge
    assert(stepFilter.findSymbol(apply).isDefined)
    assert(stepFilter.findSymbol(unapply).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val anonymousfun = FakeJdiMethod("example.Main$", "$anonfun$1")("x" -> "int")("int")
    // TODO fix: it should find the symbol f by traversing the tree of object Main
    assert(stepFilter.findSymbol(anonymousfun).isEmpty)
  }

  test("matches this.type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(): this.type = this
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.A", "m")()("example.A")
    assert(stepFilter.findSymbol(m).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val mDefault1 = FakeJdiMethod("example.A", "m$default$1")()("java.lang.String")
    val mDefault2 = FakeJdiMethod("example.A", "m$default$2")()("int")
    val initDefault1 = FakeJdiMethod("example.A$", "$lessinit$greater$default$1")()("java.lang.String")
    val initDefault2 = FakeJdiMethod("example.A$", "$lessinit$greater$default$2")()("int")

    // TODO fix: should be able to find a1, y, x and the other y by traversing the trees of m and <init>
    assert(stepFilter.findSymbol(mDefault1).isDefined)
    assert(stepFilter.findSymbol(mDefault2).isDefined)
    assert(stepFilter.findSymbol(initDefault1).isDefined)
    assert(stepFilter.findSymbol(initDefault2).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    def findM(declaringType: String, returnType: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod(declaringType, "m")("xs" -> "scala.collection.immutable.List")(returnType))

    assert(findM("example.A", "int").isDefined)
    assert(findM("example.B", "int").isEmpty)
    assert(findM("example.B", "java.lang.String").isDefined)
  }

  test("should match all kinds of types") {
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
         |object Main extends A {
         |  def m(a : example.A): example.A = a
         |  def mbis(b: A#B): A#B = b
         |  def mbis(a: A)(b: a.B): a.B = b
         |  def m(a: this.type): this.type = a
         |  def mbis(a: A { def b: B }): A { def b: B } = a
         |  def m(x: String @annot): String @annot = x
         |  def m[T](x: T): T = x
         |  def mbis(a: Main.type): Main.type = a
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)

    def assertFind(name: String)(arguments: (String, String)*)(returnType: String)(using munit.Location): Unit =
      val m = stepFilter.findSymbol(FakeJdiMethod("example.Main$", name)(arguments*)(returnType))
      assert(m.isDefined)

    assertFind("m")("a" -> "example.A")("example.A")
    assertFind("mbis")("b" -> "example.A$B")("example.A$B")
    assertFind("mbis")("a" -> "example.A", "b" -> "example.A$B")("example.A$B")
    assertFind("m")("a" -> "example.Main$")("example.Main$")
    assertFind("mbis")("a" -> "example.A")("example.A")
    assertFind("m")("x" -> "java.lang.String")("java.lang.String")
    assertFind("m")("x" -> "java.lang.Object")("java.lang.Object")
    assertFind("mbis")("a" -> "example.Main$")("example.Main$")
  }

  test("matches constant type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(x: "a"): 1 = 1
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.A", "m")("x" -> "java.lang.String")("int")
    assert(stepFilter.findSymbol(m).isDefined)
  }

  test("matches type aliases") {
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
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.Main$", "m")("x" -> "example.A")("java.lang.String")
    assert(stepFilter.findSymbol(m).isDefined)
  }

  test("matches refined types") {
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
    val stepFilter = getStepFilter(debuggee)
    val m1 = FakeJdiMethod("example.Main$", "m1")()("example.B")
    val m2 = FakeJdiMethod("example.Main$", "m2")()("java.lang.Object")
    assert(stepFilter.findSymbol(m1).isDefined)
    assert(stepFilter.findSymbol(m2).isDefined)
  }

  test("matches type parameters") {
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
    val stepFilter = getStepFilter(debuggee)
    val m1 = FakeJdiMethod("example.B", "m1")("x" -> "example.A")("example.A")
    val m2 = FakeJdiMethod("example.B", "m2")("x" -> "example.A")("example.A")
    assert(stepFilter.findSymbol(m1).isDefined)
    assert(stepFilter.findSymbol(m2).isDefined)
  }

  test("matches nested classes") {
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
    val stepFilter = getStepFilter(debuggee)
    val today = FakeJdiMethod("example.Main$", "today")()("scala.Enumeration$Value")
    assert(stepFilter.findSymbol(today).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    def findM(argumentType: String, returnType: String): Option[TermSymbol] =
      stepFilter.findSymbol(FakeJdiMethod("example.Main$", "m")("xs" -> argumentType)(returnType))
    assert(findM("int[]", "scala.runtime.Nothing$").isDefined)
    assert(findM("java.lang.String[]", "scala.runtime.Null$").isDefined)
  }

  test("matches Array[T] whose erasure is Object") {
    val source =
      """|package example
         |
         |object Main {
         |  def m[T](xs: Array[T]): Array[T] = xs
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.Main$", "m")("xs" -> "java.lang.Object")("java.lang.Object")
    assert(stepFilter.findSymbol(m).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.A", "m")("x" -> "java.lang.Object")("java.lang.Object")
    assert(stepFilter.findSymbol(m).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val aInit = FakeJdiMethod("example.A", "$init$")("$this" -> "example.A")("void")
    val bInit = FakeJdiMethod("example.B", "<init>")()("example.B")
    // TODO fix: should find the constructors and initializers
    assert(stepFilter.findSymbol(aInit).isEmpty)
    assert(stepFilter.findSymbol(bInit).isEmpty)
  }

  test("matches vararg type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(as: String*): String = as.mkString(", ")
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.A", "m")("as" -> "scala.collection.immutable.Seq")("java.lang.String")
    assert(stepFilter.findSymbol(m).isDefined)
  }

  test("matches vararg type") {
    val source =
      """|package example
         |
         |class A {
         |  def m(as: String*): String = as.mkString(", ")
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    val stepFilter = getStepFilter(debuggee)
    val m = FakeJdiMethod("example.A", "m")("as" -> "scala.collection.immutable.Seq")("java.lang.String")
    assert(stepFilter.findSymbol(m).isDefined)
  }

  test("match encoded symbols") {
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
    val stepFilter = getStepFilter(debuggee)
    val & = FakeJdiMethod("example.Main$", "$amp")("x" -> "example.$less$greater")("java.lang.String")
    val m = FakeJdiMethod("example.$less$greater", "m")()("example.$less$greater")
    assert(stepFilter.findSymbol(&).isDefined)
    assert(stepFilter.findSymbol(m).isDefined)
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
    val stepFilter = getStepFilter(debuggee)
    val rec = FakeJdiMethod("example.Main$", "rec$1")("x" -> "int", "acc" -> "int")("int")
    // TODO fix: find rec by traversing the tree of object Main
    assert(stepFilter.findSymbol(rec).isEmpty)
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
    val stepFilter = getStepFilter(debuggee)
    val lazyInitializer =
      FakeJdiMethod("example.Main$", "foo$lzyINIT1$1")("foo$lzy1$1" -> "scala.runtime.LazyRef")("java.lang.String")
    val getter =
      FakeJdiMethod("example.Main$", "foo$1")("foo$lzy1$2" -> "scala.runtime.LazyRef")("java.lang.String")
    // TODO fix: find foo by traversing the tree of object Main
    assert(stepFilter.findSymbol(lazyInitializer).isEmpty)
    assert(stepFilter.findSymbol(getter).isEmpty)
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
    val stepFilter = getStepFilter(debuggee)
    val foo = FakeJdiMethod("example.Outer", "example$Outer$$foo")()("java.lang.String")
    val m = FakeJdiMethod("example.A$", "example$A$$$m")()("int")
    assert(stepFilter.findSymbol(foo).isDefined)
    assert(stepFilter.findSymbol(m).isDefined)
  }

  private def getStepFilter(debuggee: Debuggee): ScalaStepFilterBridge =
    val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))
    }
    val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
    new ScalaStepFilterBridge(debuggeeClasspath, println, false)
