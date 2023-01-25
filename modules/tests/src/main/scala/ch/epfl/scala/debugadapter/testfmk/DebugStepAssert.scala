package ch.epfl.scala.debugadapter.testfmk

import java.nio.file.Path
import com.microsoft.java.debug.core.protocol.Types.StackFrame
import DebugStepAssert.*
import munit.Assertions.*
import munit.Location

final case class DebugStepAssert[T](step: DebugStep[T], assertion: T => Unit)

sealed trait DebugStep[+T]
final case class Breakpoint(sourceFile: Path, line: Int, condition: Option[String]) extends DebugStep[StackFrame]
final case class Logpoint(sourceFile: Path, line: Int, logMessage: String) extends DebugStep[String]
final case class StepIn() extends DebugStep[StackFrame]
final case class StepOut() extends DebugStep[StackFrame]
final case class StepOver() extends DebugStep[StackFrame]
final case class Evaluation(expression: String) extends DebugStep[Either[String, String]]
final case class Outputed() extends DebugStep[String]
final case class NoStep() extends DebugStep[Nothing]

final case class ObjectRef(clsName: String)

object DebugStepAssert {
  def assertOnFrame(expectedSource: Path, expectedLine: Int)(frame: StackFrame)(implicit location: Location): Unit = {
    assertEquals(frame.source.path, expectedSource.toString)
    assertEquals(frame.line, expectedLine)
  }

  def assertOnFrame(expectedName: String)(frame: StackFrame): Unit =
    assertEquals(frame.name, expectedName)
}

object Breakpoint {
  def apply(line: Int)(implicit ctx: TestingContext): DebugStepAssert[StackFrame] =
    Breakpoint(ctx.mainSource, line)

  def apply(sourceFile: Path, line: Int): DebugStepAssert[StackFrame] = {
    val breakpoint = Breakpoint(sourceFile, line, None)
    DebugStepAssert(breakpoint, assertOnFrame(sourceFile, line))
  }

  def apply(line: Int, condition: String)(implicit ctx: TestingContext): DebugStepAssert[StackFrame] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, Some(condition))
    DebugStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line))
  }
}

object Logpoint {
  def apply(line: Int, logMessage: String, expected: String)(implicit
      ctx: TestingContext,
      loc: Location
  ): DebugStepAssert[String] = {
    val logpoint = Logpoint(ctx.mainSource, line, logMessage)
    DebugStepAssert(logpoint, output => assertEquals(output, expected))
  }
}

object StepIn {
  def line(line: Int)(implicit ctx: TestingContext, location: Location): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepIn(), assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepIn(), assertOnFrame(methodName))
}

object StepOut {
  def line(line: Int)(implicit ctx: TestingContext): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOut(), assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOut(), assertOnFrame(methodName))
}

object StepOver {
  def line(line: Int)(implicit ctx: TestingContext): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOver(), assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOver(), assertOnFrame(methodName))
}

object Evaluation {
  def ignore(expression: String, expected: Any)(implicit ctx: TestingContext): DebugStepAssert[Either[String, String]] =
    new DebugStepAssert(Evaluation(expression), assertIgnore(expected.toString))

  def failed(expression: String, error: String): DebugStepAssert[Either[String, String]] =
    DebugStepAssert(Evaluation(expression), assertFailed(error))

  def failed(expression: String): DebugStepAssert[Either[String, String]] =
    DebugStepAssert(Evaluation(expression), resp => assertFailed(resp))

  def failedOrIgnore(expression: String, error: String, ignore: Boolean)(implicit
      ctx: TestingContext
  ): DebugStepAssert[Either[String, String]] = {
    new DebugStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore(error) _ else assertFailed(error) _
    )
  }

  def failedOrIgnore(expression: String, ignore: Boolean)(assertion: String => Unit)(implicit ctx: TestingContext) = {
    new DebugStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("failure") _ else assertFailed(assertion) _
    )
  }

  def success(expression: String, result: Any)(implicit
      ctx: TestingContext,
      location: Location
  ): DebugStepAssert[Either[String, String]] =
    new DebugStepAssert(Evaluation(expression), assertSuccess(result))

  def success(expression: String)(assertion: String => Unit): DebugStepAssert[Either[String, String]] = {
    new DebugStepAssert(Evaluation(expression), assertSuccess(assertion))
  }

  def successOrIgnore(expression: String, result: Any, ignore: Boolean)(implicit
      ctx: TestingContext
  ): DebugStepAssert[Either[String, String]] = {
    val assertion = if (ignore) assertIgnore(result.toString) _ else assertSuccess(result)(_)
    new DebugStepAssert(Evaluation(expression), assertion)
  }

  def successOrIgnore(expression: String, ignore: Boolean)(
      assertion: String => Unit
  )(implicit ctx: TestingContext): DebugStepAssert[Either[String, String]] = {
    new DebugStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("sucess") _ else assertSuccess(assertion)(_)
    )
  }

  private def assertFailed(response: Either[String, String]): Unit =
    assert(response.isLeft)

  private def assertFailed(assertion: String => Unit)(response: Either[String, String]): Unit = {
    assert(response.isLeft)
    val error = response.left.toOption.get
    assertion(error)
  }

  private def assertFailed(expectedError: String)(response: Either[String, String]): Unit = {
    assert(clue(response).isLeft)
    val error = response.left.toOption.get
    assert(clue(error).contains(clue(expectedError)))
  }

  private def assertIgnore(
      expected: String
  )(response: Either[String, String])(implicit ctx: TestingContext): Unit = {
    println(s"TODO fix in ${ctx.scalaVersion}: expected $expected")
  }

  private def assertSuccess(assertion: String => Unit)(response: Either[String, String]): Unit = {
    assert(clue(response).isRight)
    val result = response.toOption.get
    assertion(result)
  }

  private def assertSuccess(
      expectedResult: Any
  )(response: Either[String, String])(implicit ctx: TestingContext, location: Location): Unit = {
    assert(clue(response).isRight)
    val result = response.toOption.get
    expectedResult match {
      case ObjectRef(clsName) =>
        assert(result.startsWith(clsName + "@"))
      case expected: String =>
        assertEquals(result, '"'.toString + expected + '"')
      case () =>
        if (ctx.scalaVersion.isScala3) assert(result.endsWith('"'.toString + "()" + '"'))
        else assertEquals(result, "<void value>")
      case expected @ (_: Boolean | _: Byte | _: Char | _: Int | _: Long | _: Short) =>
        assertEquals(result, expected.toString)
      case floating @ (_: Double | _: Float) =>
        val expected = String.format("%f", floating.toString.toDouble: java.lang.Double)
        assertEquals(result, expected)
      case expected =>
        // they have the same toString
        assert(result.endsWith("\"" + expected + "\""))
    }
  }
}

object Outputed {
  def apply(expected: String): DebugStepAssert[String] =
    apply(message => assertEquals(message, expected))

  def apply(assertion: String => Unit): DebugStepAssert[String] =
    new DebugStepAssert(Outputed(), assertion)
}

object NoStep {
  def apply(): DebugStepAssert[Nothing] =
    new DebugStepAssert[Nothing](new NoStep(), _ => ())
}
