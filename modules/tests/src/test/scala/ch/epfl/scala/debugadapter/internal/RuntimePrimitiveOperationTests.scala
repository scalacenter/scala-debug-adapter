package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.DebugStepAssert
import ch.epfl.scala.debugadapter.testfmk.Evaluation

class RuntimePrimitiveOperationTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.3`
  val source =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val name = "world"; new java.lang.Long(42L)
       |    println(name)
       |    val foo1 = Foo(1)
       |    val foo2 = Foo(2)
       |    val barFoo1 = Bar(foo1)
       |    val barFoo2 = Bar(foo2)
       |    println(barFoo1)
       |  }
       |  def int(): Int = 42
       |  def boolean(): Boolean = true
       |  def double(): Double = 42.0
       |  def float(): Float = 42.0f
       |  def long(): Long = 42L
       |  def short(): Short = 42.toShort
       |  def byte(): Byte = 42.toByte
       |  def char(): Char = 'a'
       |}
       |
       |case class Foo(x: Int)
       |case class Bar(foo: Foo)
       |""".stripMargin
  implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("primitive operations on doubles") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("2.5 + 3.7", 6.2),
        Evaluation.success("5.0 - 2.3", 2.7),
        Evaluation.success("1.2 * 4.0", 4.8),
        Evaluation.success("7.5 / 2.5", 3.0),
        Evaluation.success("10.6 % 3.0", 1.6),
        Evaluation.success("2.1 < 3.0", true),
        Evaluation.success("3.5 <= 4.2", true),
        Evaluation.success("2.9999999999 <= 2.9999999999", true),
        Evaluation.success("3 <= 2.9999999999", false),
        Evaluation.success("5.7 > 4.9", true),
        Evaluation.success("6.0 >= 5.0", true),
        Evaluation.success("2.5 + 3.7 == 6.2", true),
        Evaluation.success("5.0 - 2.3 != 2.69", true),
        Evaluation.success("1.2 * 4.0 == 4.81", false),
        Evaluation.success("7.5 / 2.5 != 3.0", false)
      )
    )
  }

  test("primitive operations on floats") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("2.5f + 3.7f", 6.2f),
        Evaluation.success("5.0f - 2.3f", 2.7f),
        Evaluation.success("1.2f * 4.0f", 4.8f),
        Evaluation.success("7.5f / 2.5f", 3.0f),
        Evaluation.success("10.6f % 3.0f", 1.6f),
        Evaluation.success("2.1f < 3.0f", true),
        Evaluation.success("3.5f <= 4.2f", true),
        Evaluation.success("2.9999999999 <= 2.9999999999", true),
        Evaluation.success("3 <= 2.9999999999", false),
        Evaluation.success("5.7f > 4.9f", true),
        Evaluation.success("6.0f >= 5.0f", true),
        Evaluation.success("2.5f + 3.7f == 6.2f", true),
        Evaluation.success("5.0f - 2.3f != 2.69f", true),
        Evaluation.success("1.2f * 4.0f == 4.81f", false),
        Evaluation.success("7.5f / 2.5f != 3.0f", false)
      )
    )
  }

  test("primitive operations on longs") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("25L + 37L", 62L),
        Evaluation.success("50L - 23L", 27L),
        Evaluation.success("12L * 40L", 480L),
        Evaluation.success("75L / 25L", 3L),
        Evaluation.success("106L % 30L", 16L),
        Evaluation.success("21L < 30L", true),
        Evaluation.success("35L <= 42L", true),
        Evaluation.success("57L > 49L", true),
        Evaluation.success("60L >= 50L", true),
        Evaluation.success("2L + 3L == 5L", true),
        Evaluation.success("5L - 2L != 2L", true),
        Evaluation.success("2L * 4L == 4L", false),
        Evaluation.success("7L / 2L != 3L", false),
        Evaluation.success("7L / 2L != 2L", true)
      )
    )
  }

  test("primitive operations on ints") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("25 + 37", 62),
        Evaluation.success("50 - 23", 27),
        Evaluation.success("12 * 40", 480),
        Evaluation.success("75 / 25", 3),
        Evaluation.success("106 % 30", 16),
        Evaluation.success("21 < 30", true),
        Evaluation.success("35 <= 42", true),
        Evaluation.success("57 > 49", true),
        Evaluation.success("60 >= 50", true),
        Evaluation.success("2 + 3 == 5", true),
        Evaluation.success("5 - 2 != 2", true),
        Evaluation.success("2 * 4 == 4", false),
        Evaluation.success("7 / 2 != 3", false),
        Evaluation.success("7 / 2 != 2", true)
      )
    )
  }

  test("primitive operations on shorts") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("25 + 37", 62.toShort),
        Evaluation.success("50 - 23", 27.toShort),
        Evaluation.success("12 * 40", 480.toShort),
        Evaluation.success("75 / 25", 3.toShort),
        Evaluation.success("106 % 30", 16.toShort),
        Evaluation.success("21 < 30", true),
        Evaluation.success("35 <= 42", true),
        Evaluation.success("57 > 49", true),
        Evaluation.success("60 >= 50", true),
        Evaluation.success("2 + 3 == 5", true),
        Evaluation.success("5 - 2 != 2", true),
        Evaluation.success("2 * 4 == 4", false),
        Evaluation.success("7 / 2 != 3", false),
        Evaluation.success("7 / 2 != 2", true)
      )
    )
  }

  test("primitive operations on bytes") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("25 + 37", 62.toByte),
        Evaluation.success("50 - 23", 27.toByte),
        Evaluation.success("12 * 10", 120.toByte),
        Evaluation.success("75 / 25", 3.toByte)
      )
    )
  }

  test("primitive operations on chars") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("'2' + '2'", 'd'),
        Evaluation.success("'~' - '8'", 'F'),
        Evaluation.success("'x' * 2", 240),
        Evaluation.success("'Z' / 2", 45),
        Evaluation.success("'c' % 3", 0),
        Evaluation.success("'A' < 'Z'", true),
        Evaluation.success("'k' <= 'k'", true),
        Evaluation.success("'M' > 'L'", true),
        Evaluation.success("'d' >= 'c'", true),
        Evaluation.success("'a' + 'b' == 'c'", false),
        Evaluation.success("'c' - 'b' != 'b'", true),
        Evaluation.success("'c' == 'c'", true),
        Evaluation.success("'c' != 'c'", false)
      )
    )
  }

  test("primitive operations booleans") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("true && true", true),
        Evaluation.success("true && false", false),
        Evaluation.success("false && true", false),
        Evaluation.success("false && false", false),
        Evaluation.success("true || true", true),
        Evaluation.success("true || false", true),
        Evaluation.success("false || true", true),
        Evaluation.success("false || false", false),
        Evaluation.success("!true", false),
        Evaluation.success("!false", true),
        Evaluation.success("true == true", true),
        Evaluation.success("true == false", false),
        Evaluation.success("false == true", false),
        Evaluation.success("false == false", true)
      )
    )
  }

  test("operator precendence") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("2.5 + 3.7 * 2.0", 9.9),
        Evaluation.success("2.5 * 3.7 + 2.0", 11.25),
        Evaluation.success("(5.0 - 2.3) / 1.5", 1.8),
        Evaluation.success("Math.pow(2.0, 3.0)", 8.0),
        Evaluation.success("Math.sqrt(25.0)", 5.0),
        Evaluation.success("(true && false) || (false && true)", false),
        Evaluation.success("(true && true) || (false && true)", true),
        Evaluation.success("(true && false) || (true && true)", true),
        Evaluation.success("(false && true) || (false && false)", false),
        Evaluation.success("!((true && false) || (false && true))", true),
        Evaluation.success("!((true && true) || (false && true))", false),
        Evaluation.success("!((true && false) || (true && true))", false),
        Evaluation.success("!((false && true) || (false && false))", true)
      )
    )
  }

  test("mixed numeric operands") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("2.5 + 3.7f", 6.2),
        Evaluation.success("2.5 + 5", 7.5),
        Evaluation.success("2.5 + 25L", 27.5),
        Evaluation.success("2.5 + 'a'", 99.5),
        Evaluation.success("3.7f + 2.5", 6.2),
        Evaluation.success("3.7f + 5", 8.7),
        Evaluation.success("3.7f + 25L", 28.7),
        Evaluation.success("3.7f + 'a'", 100.7),
        Evaluation.success("25L + 2.5", 27.5),
        Evaluation.success("25L + 3.7f", 28.7),
        Evaluation.success("25L + 5", 30L),
        Evaluation.success("25L + 'a'", 122L),
        Evaluation.success("5 + 2.5", 7.5),
        Evaluation.success("5 + 3.7f", 8.7),
        Evaluation.success("5 + 25L", 30L),
        Evaluation.success("5 + 'a'", 102),
        Evaluation.success("'a' + 2.5", 99.5),
        Evaluation.success("'a' + 3.7f", 100.7),
        Evaluation.success("'a' + 5", 102),
        Evaluation.success("'a' + 25L", 122L)
      )
    )
  }

  test("mix of literals and computed values") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("double() + 1", 43.0),
        Evaluation.success("float() + 1", 43.0f),
        Evaluation.success("long() + 1", 43L),
        Evaluation.success("int() + 1", 43),
        Evaluation.success("short() + 1", 43.toShort),
        Evaluation.success("byte() + 1", 43.toByte),
        Evaluation.success("char() + 1", 98),
        Evaluation.success("boolean() || false", true)
      )
    )
  }

  test("unbox to primitive values") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("new java.lang.Integer(42) + 1", 43),
        Evaluation.success("1 + new java.lang.Integer(42)", 43),
        Evaluation.success("new java.lang.Integer(42) + new java.lang.Integer(1)", 43),
        Evaluation.success("new java.lang.Long(42L) + 1", 43L),
        Evaluation.success("1 + new java.lang.Long(42L)", 43L),
        Evaluation.success("new java.lang.Long(42L) + new java.lang.Integer(1)", 43L),
        Evaluation.success("new java.lang.Float(42f) + 1", 43.0f),
        Evaluation.success("1 + new java.lang.Float(42f)", 43.0f),
        Evaluation.success("new java.lang.Double(42.0) + 1", 43.0),
        Evaluation.success("1 + new java.lang.Double(42.0)", 43.0),
        Evaluation.success("new java.lang.Character('a') + 1", 98),
        Evaluation.success("1 + new java.lang.Character('a')", 98),
        Evaluation.success("new java.lang.Boolean(true) || false", true),
        Evaluation.success("true || new java.lang.Boolean(false)", true)
      )
    )
  }

  test("equality") {
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("Foo(1) == Foo(1)", true),
        Evaluation.success("Foo(1) == Foo(2)", false),
        Evaluation.success("Foo(1) != Foo(1)", false),
        Evaluation.success("Foo(1) != Foo(2)", true),
        Evaluation.success("foo1 == foo1", true),
        Evaluation.success("foo1 == foo2", false),
        Evaluation.success("foo1 != foo1", false),
        Evaluation.success("foo1 != foo2", true),
        Evaluation.success("Bar(Foo(1)) == Bar(Foo(1))", true),
        Evaluation.success("Bar(Foo(1)) == Bar(Foo(2))", false),
        Evaluation.success("Bar(Foo(1)) != Bar(Foo(1))", false),
        Evaluation.success("Bar(Foo(1)) != Bar(Foo(2))", true),
        Evaluation.success("barFoo1 == barFoo1", true),
        Evaluation.success("barFoo1 == barFoo2", false),
        Evaluation.success("barFoo1 != barFoo1", false),
        Evaluation.success("barFoo1 != barFoo2", true)
      )
    )
  }

  test("type-check operands") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.failed("1 + new java.lang.Object()"),
        Evaluation.failed("new java.lang.Object() + 1"),
        Evaluation.failed("!1"),
        Evaluation.failed("1 && true"),
        Evaluation.failed("true && 1"),
        Evaluation.failed("1 + true"),
        Evaluation.failed("true + 1")
      )
    )
  }

  test("unary operations") {
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("+1 + 1", 2),
        Evaluation.success("+1L", 1L),
        Evaluation.success("+1f", 1.0f),
        Evaluation.success("+1.0", 1.0),
        Evaluation.success("-1", -1),
        Evaluation.success("-1L", -1L),
        Evaluation.success("-1f", -1.0f),
        Evaluation.success("-1.0", -1.0),
        Evaluation.success("~1", -2),
        Evaluation.success("~1L", -2L)
      )
    )
  }
}
