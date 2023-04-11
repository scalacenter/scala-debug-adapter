package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.DebugStepAssert
import ch.epfl.scala.debugadapter.testfmk.Evaluation

object RuntimeEvaluatorPrimitiveEnvironment {
  val primitiveSource =
    """|package example
       |
       |object Main {
       |  def main(args: Array[String]): Unit = {
       |    val name = "world"
       |    println(name)
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
       |""".stripMargin
}

class Scala212RuntimeEvaluatorPrimitiveOperationTests
    extends RuntimeEvaluatorPrimitiveOperationTests(ScalaVersion.`2.12`)
class Scala213RuntimeEvaluatorPrimitiveOperationTests
    extends RuntimeEvaluatorPrimitiveOperationTests(ScalaVersion.`2.13`)
class Scala3RuntimeEvaluatorPrimitiveOperationTests extends RuntimeEvaluatorPrimitiveOperationTests(ScalaVersion.`3.0`)
class Scala31RuntimeEvaluatorPrimitiveOperationTests
    extends RuntimeEvaluatorPrimitiveOperationTests(ScalaVersion.`3.1+`)

abstract class RuntimeEvaluatorPrimitiveOperationTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
  lazy val localVar =
    TestingDebuggee.mainClass(RuntimeEvaluatorPrimitiveEnvironment.primitiveSource, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("Should compute primitive method calls on doubles") {
    implicit val debuggee = localVar
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

  test("Should compute primitive method calls on floats") {
    implicit val debuggee = localVar
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

  test("Should compute primitive method calls on longs") {
    implicit val debugge = localVar
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

  test("Should compute primitive method calls on ints") {
    implicit val debugge = localVar
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

  test("Should compute primitive method calls on shorts") {
    implicit val debugge = localVar
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

  test("Should compute primitive method calls on bytes") {
    implicit val debugge = localVar
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

  test("Should compute primitive method calls on chars") {
    implicit val debugge = localVar
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

  test("Should compute primitive method calls on booleans") {
    implicit val debugge = localVar
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

  test("Should take accounts of order of priorities") {
    implicit val debuggee = localVar
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

  test("Should compute mixed operands type primitive numeric operations") {
    implicit val debuggee = localVar
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

  test("Should compute a mix of literal primitive values and computed primitive values") {
    implicit val debuggee = localVar
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

  test("Should unbox types supporting primitive operations") {
    implicit val debuggee = localVar
    check(
      Breakpoint(6),
      DebugStepAssert.inParallel(
        Evaluation.success("new java.lang.Integer(42) + 1", 43),
        Evaluation.success("new java.lang.Long(42L) + 1", 43L),
        Evaluation.success("new java.lang.Float(42f) + 1", 43.0f),
        Evaluation.success("new java.lang.Double(42.0) + 1", 43.0),
        Evaluation.success("new java.lang.Character('a') + 1", 98),
        Evaluation.success("new java.lang.Boolean(true) || false", true)
      )
    )
  }
}
