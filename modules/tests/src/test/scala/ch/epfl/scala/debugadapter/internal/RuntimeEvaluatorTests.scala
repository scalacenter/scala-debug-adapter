package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.*
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.DebugConfig

// TODO: test if it falls back to compiler when runtime validation fails, but not when parsing / evaluation fails
object RuntimeEvaluatorEnvironments {
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
       |  def keeSuperfoo: String = superfoo
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
       |    Foo_v2.hello
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
       |trait FooTrait { def bar1: Int = 42 }
       |
       |case class Foo_v1() extends FooTrait {
       |  def foo: String = "hello foo"
       |  protected def foo_v2 = Foo_v2
       |  def foo_v2_apply = Foo_v2("hello ")
       |  def bar(str: String, int: Int): String = str + int
       |  case class InnerFoo(x: Int) { def hello(x1: Int) = s"hello inner foo ${x + x1}" }
       |  object InnerFoo { val hello: String = "hello inner foo" }
       |  def unary_+ : String = "hello unary"
       |}
       |
       |object Foo_v1 { def hello: String = "hello foo" }
       |
       |case class Foo_v2(str: String) { def bar(int: Int): String = str + int }
       |
       |object Foo_v2 { val hello: String = "hello foo" }
       |""".stripMargin

  val nested =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val foo = Foo()
       |    val inner = Inner(42)
       |    val friendFoo = foo.FriendFoo(foo)
       |    println("the end")
       |  }
       |
       |  case class Inner(x: Int) {
       |    val y = x + 1
       |    def helloInner = s"hello inner $x"
       |  }
       |  object Inner { 
       |    val z = 42
       |    def helloInner = "hello inner"
       |  }
       |}
       |
       |case class Foo() {
       |  case class FriendFoo(f1: Foo) {
       |    val y = 42
       |    def greet = "Friend"
       |    def add(x: Int, y: Int)= x + y
       |  }
       |  object FriendFoo {
       |    val z = 42
       |    def greet = "Friendly"
       |  }
       |}
       |
    """.stripMargin
}

class Scala212RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`2.12`)
class Scala213RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`2.13`)
class Scala3RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`3.0`)
class Scala31RuntimeEvaluatorTests extends RuntimeEvaluatorTests(ScalaVersion.`3.1+`)

class Scala212RuntimeEvaluatorFallbackTests extends RuntimeEvaluatorFallbackTests(ScalaVersion.`2.12`)
class Scala213RuntimeEvaluatorFallbackTests extends RuntimeEvaluatorFallbackTests(ScalaVersion.`2.13`)
class Scala3RuntimeEvaluatorFallbackTests extends RuntimeEvaluatorFallbackTests(ScalaVersion.`3.0`)
class Scala31RuntimeEvaluatorFallbackTests extends RuntimeEvaluatorFallbackTests(ScalaVersion.`3.1+`)

abstract class RuntimeEvaluatorTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val localVar =
    TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.localVarTestSource, "example.Main", scalaVersion)
  lazy val field = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.fieldSource, "example.Main", scalaVersion)
  lazy val method = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.methodSource, "example.Main", scalaVersion)
  lazy val nested = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.nested, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("should retrieve the value of a local variable from jdi --- scala") {
    implicit val debuggee = localVar
    check(Breakpoint(6), Evaluation.success("name", "world"), Evaluation.failed("unknown"))

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

  test("Should compute a method call on the current class --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(28),
      Evaluation.success("bar1", 42),
      Breakpoint(11),
      Evaluation.success("foo", "hello foo")
    )
  }

  test("Should compute a method call on an instance of a class --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
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
      Breakpoint(11),
      Evaluation.success("+f1", "hello unary")
    )
  }

  test("Should fail if the method doest not exists or arguments aren't correct (in count & types) --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.failed("f1.bar(\"hello \", 42, 42)"),
        Evaluation.failed("f1.bar(\"hello \")"),
        Evaluation.failed("f1.bar(42, 42)")
      )
    )
  }

  test("Should compute an implicit .apply() call --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("f1.foo_v2(\"hello \").bar(42)", "hello 42"),
        Evaluation.success("Foo_v1().toString()", "Foo_v1()"),
        Evaluation.success("f1.foo_v2_apply.bar(42)", "hello 42"),
        Evaluation.success("inner(42).x", 42)
      )
    )
  }

  test("Should find the right inner type among the same-name inner types --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("InnerFoo(41).hello(1)", "hello inner foo 42"),
        Evaluation.success("InnerFoo.hello", "hello main inner foo")
      )
    )
  }

  test("Should be able to unbox arguments --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("inner(new Integer(42)).x", 42),
        Evaluation.failed("inner(new Boolean(true)).x")
      )
    )
  }

  test("Should compute a static method call --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
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
      Breakpoint(11),
      Evaluation.success("bar(\"hello \", 42)", "hello 42"),
      Evaluation.failed("bar(List(1, 2, 3))")
    )
  }

  test("Should get the value of a field in a nested type --- scala") {
    implicit val debuggee = nested
    check(
      Breakpoint(8),
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
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.success("Inner.helloInner", "hello inner"),
        Evaluation.success("inner.helloInner", "hello inner 42"),
        Evaluation.success("foo.FriendFoo.greet", "Friendly"),
        Evaluation.success("foo.FriendFoo(foo).greet", "Friend"),
        Evaluation.success("foo.FriendFoo(foo).add(1, 2)", 3)
      )
    )
  }

  test("Should not access inner / nested types of a class") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
      Evaluation.failed("Foo_v1.InnerFoo(41).hello(1)"),
      Evaluation.failed("Foo_v1.InnerFoo.hello")
    )
  }
}

abstract class RuntimeEvaluatorFallbackTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val localVar =
    TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.localVarTestSource, "example.Main", scalaVersion)
  lazy val field = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.fieldSource, "example.Main", scalaVersion)
  lazy val method = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.methodSource, "example.Main", scalaVersion)
  lazy val nested = TestingDebuggee.mainClass(RuntimeEvaluatorEnvironments.nested, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.ScalaEvaluationOnly)

  test("Should fallback to compiler when overloads are present --- scala") {
    implicit val debuggee = method
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("bar(\"hello \", 42)", "hello 42"),
        Evaluation.success("bar(42, 42)", 84)
      )
    )
  }
}
