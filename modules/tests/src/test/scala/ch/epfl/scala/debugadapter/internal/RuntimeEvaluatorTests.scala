package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.*
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.DebugConfig

object RuntimeEvaluatorEnvironments {
  val byNameFunction0 =
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
  val localVarTestSource =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val name = "world"
       |    println(name)
       |  }
       |}
       |""".stripMargin

  val fieldSource =
    """|package example
       |
       |object Main {
       |  val lapin = "lapin"
       |
       |  def main(args: Array[String]): Unit = {
       |    val f1 = Foo()
       |    f1.bar("hello ", 42)
       |  }
       |}
       |
       |trait FooTrait { val foo1: String = "hello there" }
       |
       |class SuperFoo { 
       |  private val superfoo: String = "hello super"
       |  def keepSuperfoo: String = superfoo
       |}
       |
       |case class Foo() extends SuperFoo with FooTrait {
       |  private val lapinou = "lapinou"
       |  def bar(str: String, int: Int): String = str + int
       |  def keepTheRabbit: String = lapinou
       |}
       |
       |object Foo { val foofoo = "foofoo" }
       |""".stripMargin

  val methodSource =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val f1 = Foo_v1()
       |    val x = f1.InnerFoo.hello
       |    f1.foo
       |    val f2 = Foo_v2("hello ")
       |    val list = List(0)
       |    println("ok")
       |  }
       |
       |  def foo: String = "hello foo"
       |  def bar(str: String, int: Int): String = str + int
       |  def bar(i1: Int, i2: Int): Int = i1 + i2
       |  def barList(list: List[Int]): Int = list.sum
       |  def barList(list: List[String]): String = list.mkString
       |  def inner = InnerFoo
       |
       |  case class InnerFoo(x: Int) { def hello(x1: Int) = s"hello inner foo ${x + x1}" }
       |  object InnerFoo { def hello: String = "hello main inner foo" }
       |}
       |
       |trait FooTrait { 
       |  val x = 3  
       |  def bar1: Int = 42
       |}
       |
       |case class Foo_v1() extends FooTrait {
       |  def foo: String = "hello foo"
       |  protected def foo_v2 = Foo_v2
       |  def foo_v2_apply = Foo_v2("hello ")
       |  def bar(str: String, int: Int): String = str + int
       |  case class InnerFoo(x: Int) { def hello(x1: Int) = s"hello inner foo ${x + x1}" }
       |  object InnerFoo {
       |    val keepOuter = foo_v2
       |    val fooTrait = x
       |    val hello: String = "hello inner foo"
       |  }
       |  class InnerFooClass { val hello: String = "hello inner foo class" }
       |  object InnerFooObject { val hello: String = "hello inner foo object" }
       |  def unary_+ : String = "hello unary"
       |}
       |
       |object Foo_v1 { def hello: String = "hello foo" }
       |
       |case class Foo_v2(str: String) { def bar(int: Int): String = str + int }
       |
       |object Foo_v2 { val hello: String = "hello foo" }
       |
       |class NoObjectFoo { def foo: String = "hello foo" }
       |""".stripMargin

  val hierarchyOverload =
    """|package example
       |
       |trait CoolTrait
       |trait SubCoolTrait extends CoolTrait
       |class Foo
       |class Bar extends Foo with CoolTrait
       |class Baz extends Foo with SubCoolTrait
       |class SubBar extends Bar
       |class SubCool extends SubCoolTrait
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val foo = new Foo()
       |    val bar = new Bar()
       |    val baz = new Baz()
       |    val subBar = new SubBar()
       |    val subCool = new SubCool()
       |    println("ok")
       |  }
       |  
       |  def test(foo: Foo): String = "foo"
       |  def test(bar: Bar): String = "bar"
       |  def test(baz: Baz): String = "baz"
       |
       |  def test(foo: Foo, subBar: SubBar): String = "foo, subBar"
       |  def test(bar: Bar, foo: Foo): String = "bar, foo"
       |
       |  def test(bar: Bar, baz: Baz): String = "bar, baz"
       |
       |  def test(foo: Foo, subCool: SubCoolTrait): String = "foo, subCool"
       |  def test(bar: Bar, subCool: SubCoolTrait): String = "bar, subCool"
       |  def test(baz: Baz, subCool: SubCoolTrait): String = "baz, subCool"
       |
       |  def test(foo: Foo, bar: Bar, baz: Baz): String = "foo, bar, baz"
       |  def test(bar: Bar, baz: Baz, subBar: SubBar): String = "bar, baz, subBar"
       |}
    """.stripMargin

  val nested =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val foo = Foo()
       |    val inner = Inner(42)
       |    inner.helloInner
       |    val friendFoo = foo.FriendFoo(foo)
       |    friendFoo.greet; inner.InnerInner().str; inner.InnerInner.str
       |    val nested1 = Nested(42);
       |    val nested2 = Nested(1);
       |    println("ok")
       |  }
       |
       |  def upperMain1 = "upper main 1"
       |  def upperMain2 = "upper main 2"
       |  def upperMain3 = "upper main 3"
       |
       |  case class Inner(x: Int) {
       |    val y = x + 1
       |    def helloInner = s"hello inner $x"
       |    case class InnerInner() { 
       |      val str = {
       |        println("ok")
       |        s"inner inner $y"
       |      } 
       |    }
       |    object InnerInner {
       |      val str = {
       |        println("ok")
       |        s"inner inner $x"
       |      }
       |    }
       |  }
       |  object Inner { 
       |    val z = 42
       |    def helloInner = "hello inner"
       |    case class DoubleInner(zz: Int) { val str = s"double inner $zz"}
       |    object DoubleInner { val str = "double inner" }
       |  }
       |}
       |
       |case class Foo() {
       |  case class FriendFoo(f1: Foo) {
       |    val y = 42
       |    def greet = "Friend"
       |    def add(x: Int, y: Int)= x + y
       |    case class InnerFriendFoo() { val str = s"inner friend foo $y"}
       |    object InnerFriendFoo { val str = s"object inner friend foo $y"}
       |  }
       |  object FriendFoo {
       |    val z = 42
       |    def greet = "Friendly"
       |    case class ObjectFriendFoo() { val str = s"object friend foo $z"}
       |    object ObjectFriendFoo { val str = s"object object friend foo $z"}
       |  }
       |  def friendFoo = FriendFoo
       |}
       |
       |case class Nested(x: Int) {
       |  object InnerNested { val y = 42 + x }
       |}
       |
    """.stripMargin

  val cls =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val foo = Foo(2)
       |    println("ok")
       |  }
       |}
       |
       |class NoObject
       |
       |case class Foo(x: Int)
       |""".stripMargin

  val boxingOverloads =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    println("ok")
       |  }
       |  
       |  def test(i: Int): String = "primitive int"
       |  def test(i: java.lang.Integer): String = "boxed int"
       |}
       |""".stripMargin
}

abstract class RuntimeEvaluatorTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val localVar =
    TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.localVarTestSource, "example.Main", scalaVersion)
  lazy val field = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.fieldSource, "example.Main", scalaVersion)
  lazy val method = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.methodSource, "example.Main", scalaVersion)
  lazy val overloads =
    TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.hierarchyOverload, "example.Main", scalaVersion)
  lazy val nested = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.nested, "example.Main", scalaVersion)
  lazy val cls = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.cls, "example.Main", scalaVersion)
  lazy val boxingOverloads =
    TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.boxingOverloads, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("Should not compute by-name param or Function0 expression") {
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.byNameFunction0, "example.Main", scalaVersion)
    check(Breakpoint(10), Evaluation.failed("x"), Breakpoint(13), Evaluation.failed("x"))
  }

  test("should retrieve the value of a local variable from jdi --- scala") {
    implicit val debuggee = localVar
    check(
      Breakpoint(6),
      Evaluation.success("name", "world"),
      Evaluation.failed("unknown")
    )

  }

  test("""|Should retrieve the value of a field
          | -> From the current class
          | -> From an instance of class
          | -> From a static field --- scala""".stripMargin) {
    implicit val debuggee = field
    check(
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.success("lapin", "lapin"),
        Evaluation.success("f1.lapinou", "lapinou"),
        Evaluation.success("f1.foo1", "hello there"),
        Evaluation.success("f1.superfoo", "hello super"),
        Evaluation.success("Foo.foofoo", "foofoo")
      )
    )
  }

  test("Should resolve non-generic overloads --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("bar(\"hello \", 42)", "hello 42"),
        Evaluation.success("bar(42, 42)", 84)
      )
    )
  }

  test("Should compute a method call on the current class --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(38),
      Evaluation.success("bar1", 42),
      Breakpoint(10),
      Evaluation.success("foo", "hello foo")
    )
  }

  test("Should compute a method on a superclass --- scala") {
    implicit val debuggee = field
    check(
      Breakpoint(8),
      Evaluation.success("f1.keepSuperfoo", "hello super")
    )
  }

  test("Should compute a method call on an instance of a class --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("f1.foo", "hello foo"),
        Evaluation.success("f1.bar(\"hello \", 42)", "hello 42"),
        Evaluation.failed("f1.bar(\"hello \", 42, 42)"),
        Evaluation.failed("f1.bar(\"hello \")"),
        Evaluation.failed("f1.bar(42, 42)"),
        Evaluation.success("f2.bar(42)", "hello 42"),
        Evaluation.success("f2 bar 42", "hello 42"),
        Evaluation.successOrIgnore("(1 +: list).toString()", "List(1, 0)", scalaVersion == ScalaVersion.`2.12`)
      )
    )
  }

  test("Should compute an unary method --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      Evaluation.success("+f1", "hello unary")
    )
  }

  test("Should fail if the method doest not exists or arguments aren't correct (in count & types) --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.failed("f1.bar(\"hello \", 42, 42)"),
        Evaluation.failed("f1.bar(\"hello \")"),
        Evaluation.failed("f1.bar(42, 42)")
      )
    )
  }

  test("Should compute non generic hierarchy overload") {
    implicit val debuggee = overloads
    check(
      Breakpoint(18),
      DebugStepAssert.inParallel(
        Evaluation.success("test(foo)", "foo"),
        Evaluation.success("test(bar)", "bar"),
        Evaluation.success("test(baz)", "baz"),
        Evaluation.failed("test(bar, subBar)"),
        Evaluation.success("test(bar, baz)", "bar, baz"),
        Evaluation.success("test(foo, subBar)", "foo, subBar"),
        Evaluation.success("test(foo, subCool)", "foo, subCool"),
        Evaluation.success("test(bar, subCool)", "bar, subCool"),
        Evaluation.success("test(baz, subCool)", "baz, subCool"),
        Evaluation.success("test(foo, bar, baz)", "foo, bar, baz"),
        Evaluation.success("test(bar, baz, subBar)", "bar, baz, subBar")
      )
    )
  }

  test("Should compute an implicit .apply() call --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("f1.foo_v2(\"hello \").bar(42)", "hello 42"),
        Evaluation.success("Foo_v1().toString()", "Foo_v1()"),
        Evaluation.success("f1.foo_v2_apply.bar(42)", "hello 42"),
        Evaluation.success("inner(42).x", 42),
        Evaluation.success("list(0).toString()", "0"),
        Evaluation.failed("list(1)")
      )
    )
  }

  test("Should work on arrays") {
    val arraysSource =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val arr = Array(1, 2, 3)
         |    val sh: Short = 2
         |    val ch: Char = 2
         |    val by: Byte = 2
         |    println("ok")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee = TestingDebuggee.mainClass(arraysSource, "example.Main", scalaVersion)
    check(
      Breakpoint(9),
      Evaluation.success("arr(0)", 1),
      Evaluation.success("arr(2)", 3),
      Evaluation.success("arr(sh)", 3),
      Evaluation.success("arr(ch)", 3),
      Evaluation.success("arr(by)", 3),
      Evaluation.success("arr(new Integer(2))", 3),
      Evaluation.success("arr(new Character('\u0000'))", 1),
      Evaluation.failed("arr(3)")
    )
  }

  test("Should work on collections") {
    val collectionSource =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val list = List(1, 2, 3)
         |    val map = Map(1 -> "one", 2 -> "two")
         |    val set = Set(1, 2, 3)
         |    val seq = Seq(1, 2, 3)
         |    val vector = Vector(1, 2, 3)
         |    println("ok")
         |  } 
         |}
         |""".stripMargin
    implicit val debuggee = TestingDebuggee.mainClass(collectionSource, "example.Main", scalaVersion)
    check(
      Breakpoint(10),
      Evaluation.success("list(0).toString", "1"),
      Evaluation.success("list(2).toString", "3"),
      Evaluation.success("list(new Integer(2)).toString", "3"),
      Evaluation.successOrIgnore("list(new Character('\u0000')).toString", "1", true),
      Evaluation.failed("list(3)"),
      Evaluation.success("map(1)", "one"),
      Evaluation.success("map(2)", "two"),
      Evaluation.success("map(new Integer(2))", "two"),
      Evaluation.successOrIgnore("map(new Character('\u0000'))", "one", true),
      Evaluation.failed("map(3)"),
      Evaluation.success("set(1)", true),
      Evaluation.success("set(2)", true),
      Evaluation.success("set(new Integer(2))", true),
      Evaluation.successOrIgnore("set(new Character('\u0000')).toStrign", "1", true),
      Evaluation.success("set(4)", false),
      Evaluation.success("seq(0).toString", "1"),
      Evaluation.success("seq(2).toString", "3"),
      Evaluation.success("seq(new Integer(2)).toString", "3"),
      Evaluation.successOrIgnore("seq(new Character('\u0000')).toString", "1", true),
      Evaluation.failed("seq(3)"),
      Evaluation.success("vector(0).toString", "1"),
      Evaluation.success("vector(2).toString", "3"),
      Evaluation.success("vector(new Integer(2)).toString", "3"),
      Evaluation.successOrIgnore("vector(new Character('\u0000'))", 1, true)
    )
  }

  test("Should find the right inner type among the same-name inner types --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("InnerFoo(41).hello(1)", "hello inner foo 42"),
        Evaluation.success("InnerFoo.hello", "hello main inner foo")
      )
    )
  }

  test("Should be able to unbox arguments --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("inner(new Integer(42)).x", 42),
        Evaluation.failed("inner(new Boolean(true)).x")
      )
    )
  }

  test("Should compute a static method call --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      Evaluation.success("Foo_v1.hello", "hello foo"),
      Evaluation.success("Foo_v2.hello", "hello foo"),
      Evaluation.success("InnerFoo.hello", "hello main inner foo")
    )
  }

  test(
    "Should fail when method overloaded with the same types after erasure are present. Should successfully compute a method overloaded with different types after erasure --- scala"
  ) {
    implicit val debuggee = method
    check(
      Breakpoint(10),
      Evaluation.success("bar(\"hello \", 42)", "hello 42"),
      Evaluation.failed("bar(List(1, 2, 3))")
    )
  }

  test("Should find outer methods, fields, class & modules") {
    implicit val debuggee = method
    check(
      Breakpoint(38),
      DebugStepAssert.inParallel(
        Evaluation.success("foo_v2(\"hello \").bar(42)", "hello 42"),
        Evaluation.success("x", 3),
        Evaluation.successOrIgnore("(new InnerFooClass).hello", "hello inner foo class", true),
        Evaluation.success("InnerFooObject.hello", "hello inner foo object")
      )
    )
  }

  test("Should get the value of a field in a nested type --- scala") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("Inner.z", 42),
        Evaluation.success("inner.y", 43),
        Evaluation.success("inner.x", 42),
        Evaluation.success("foo.FriendFoo.z", 42),
        Evaluation.success("foo.FriendFoo(foo).y", 42)
      )
    )
  }

  test("Should compute a method call on a nested type --- scala") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("Inner.helloInner", "hello inner"),
        Evaluation.success("inner.helloInner", "hello inner 42"),
        Evaluation.success("foo.FriendFoo.greet", "Friendly"),
        Evaluation.success("foo.FriendFoo(foo).greet", "Friend"),
        Evaluation.success("foo.FriendFoo(foo).add(1, 2)", 3),
        Evaluation.success("Main.Inner.DoubleInner.str", "double inner")
      )
    )
  }

  test("Should not access inner / nested types of a non-static class") {
    implicit val debuggee = nested
    check(
      Breakpoint(10),
      Evaluation.failed("Foo.FriendFoo.greet"),
      Evaluation.failed("Foo.FriendFoo(Foo()).greet"),
      Evaluation.failed("Foo().friendFoo.InnerFriendFoo.str"),
      Evaluation.failed("Foo().friendFoo.InnerFriendFoo().str")
    )
  }

  test("Should access to multiple layers of nested types") {
    implicit val debuggee = nested
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("Main.Inner.helloInner", "hello inner"),
        Evaluation.success("Main.Inner(41).y", 42),
        Evaluation.success("Main.Inner.z", 42),
        Evaluation.success("Main.Inner.DoubleInner(84).str", "double inner 84"),
        Evaluation.success("Main.Inner.DoubleInner.str", "double inner"),
        Evaluation.success("Main.Inner(42).InnerInner.str", "inner inner 42"),
        Evaluation.success("Main.Inner(41).InnerInner().str", "inner inner 42"),
        Evaluation.success("Foo().FriendFoo(Foo()).InnerFriendFoo.str", "object inner friend foo 42"),
        Evaluation.success("Foo().FriendFoo(Foo()).InnerFriendFoo().str", "inner friend foo 42"),
        Evaluation.success("Foo().FriendFoo.ObjectFriendFoo().str", "object friend foo 42"),
        Evaluation.success("Foo().FriendFoo.ObjectFriendFoo.str", "object object friend foo 42")
      )
    )
  }

  test("Should access the right nested module") {
    implicit val debuggee = nested
    check(
      Breakpoint(12),
      DebugStepAssert.inParallel(
        Evaluation.success("nested1.InnerNested.y", 84),
        Evaluation.success("nested2.InnerNested.y", 43),
        Evaluation.success("Nested(0).InnerNested.y", 42),
        Evaluation.success("Nested(100).InnerNested.y", 142),
        Evaluation.success("Nested(42).InnerNested.y", 84),
        Evaluation.success("Nested(84).InnerNested.y", 126),
        Evaluation.failed("Nested.InnerNested.y")
      )
    )
  }

  test("Should evaluate a module, but not a class") {
    implicit val debuggee = cls
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("foo.getClass") { res => assert(res.startsWith("Class (Foo)@")) },
        Evaluation.success("Foo") { res => assert(res.startsWith("Foo$@")) },
        Evaluation.failed("NoObject")
      )
    )
  }

  test("Should resolve overloads in 2 steps when boxing is available") {
    implicit val debuggee = boxingOverloads
    check(
      Breakpoint(5),
      Evaluation.success("test(1)", "primitive int"),
      Evaluation.success("test(new Integer(1))", "boxed int")
    )
  }

  test("Should instantiate inner classes") {
    val source =
      """|package example
         |
         |class A {
         |  class AA {
         |    class AAA(val x: Int)
         |  }
         |  object AA {
         |    class StaticAAA
         |  }
         |}
         |
         |object A {
         |  class StaticAA {
         |    class AAA
         |  }
         |  object StaticAA {
         |    class StaticAAA
         |  }
         |}
         |
         |object Main {
         |  val AStaticAA = new A.StaticAA
         |  def main(args: Array[String]): Unit = {
         |    val a = new A
         |    val aAA = new a.AA
         |    val aAAaaa1 = new aAA.AAA(42)
         |    val aAAaaa2 = new aAA.AAA(43)
         |    println("ok")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(28),
      Evaluation.success("new a.AA") { res => res.startsWith("A$AA@") },
      Evaluation.success("new aAA.AAA(42)") { res => res.startsWith("A$AA$AAA@") },
      Evaluation.success("new a.AA.StaticAAA") { res => res.startsWith("A$AA$StaticAAA@") },
      Evaluation.success("new A.StaticAA") { res => res.startsWith("A$StaticAA@") },
      Evaluation.success("new AStaticAA.AAA") { res => res.startsWith("A$StaticAA$AAA@") },
      Evaluation.success("new this.AStaticAA.AAA") { res => res.startsWith("A$StaticAA$AAA@") },
      Evaluation.success("new A.StaticAA.StaticAAA") { res => res.startsWith("A$StaticAA$StaticAAA@") },
      Evaluation.success("aAAaaa1.x", 42),
      Evaluation.success("aAAaaa2.x", 43)
    )
  }

  test("Should pre-evaluate $outer") {
    val source =
      """|package example
         |
         |class A {
         |  val x = 42
         |  class C
         |  object C { def life = x }
         |}
         |
         |class B extends A {
         |  val y = 43
         |}
         |
         |object B extends A {
         |  val y = 84
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    val bc = new b.C
         |    val bC = b.C
         |    val Bc = new B.C
         |    val BC = B.C
         |    println("ok")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(24),
      Evaluation.success("bc.y", 43),
      Evaluation.success("bC.y", 43),
      Evaluation.success("Bc.y", 84),
      Evaluation.success("BC.y", 84)
    )
  }
}

/* -------------------------------------------------------------------------- */
/*                        Scala version specific tests                        */
/* -------------------------------------------------------------------------- */

class Scala212RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`2.12`) {
  test("Should access to wrapping 'object' methods") {
    implicit val debuggee = nested
    check(
      Breakpoint(21),
      Evaluation.success("upperMain1", "upper main 1"),
      Breakpoint(25),
      Evaluation.success("upperMain2", "upper main 2"),
      Breakpoint(31),
      Evaluation.success("upperMain3", "upper main 3")
    )
  }
}
class Scala213RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`2.13`) {
  test("Should access to wrapping 'object' methods") {
    implicit val debuggee = nested
    check(
      Breakpoint(21),
      Evaluation.success("upperMain1", "upper main 1"),
      Breakpoint(25),
      Evaluation.success("upperMain2", "upper main 2"),
      Breakpoint(31),
      Evaluation.success("upperMain3", "upper main 3")
    )
  }
}
class Scala31RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`3.1+`) {
  test("Should access to wrapping 'object' methods") {
    implicit val debuggee = nested
    check(
      Breakpoint(21),
      Evaluation.success("upperMain1", "upper main 1"),
      Breakpoint(31),
      Evaluation.success("upperMain2", "upper main 2"),
      Breakpoint(25),
      Evaluation.success("upperMain3", "upper main 3")
    )
  }

  test("Should evaluate a local variable of a lambda") {
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
      Breakpoint(8), // regression in Scala 3.2.2
      Breakpoint(9), // regression in Scala 3.2.2
      Breakpoint(13), // calling withFilter
      Breakpoint(13),
      Evaluation.success("x", 1)
    )
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
}
