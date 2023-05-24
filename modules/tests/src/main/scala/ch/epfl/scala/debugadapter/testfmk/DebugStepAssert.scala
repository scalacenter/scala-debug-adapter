package ch.epfl.scala.debugadapter.testfmk

import java.nio.file.Path
import com.microsoft.java.debug.core.protocol.Types.StackFrame
import DebugStepAssert.*
import munit.Assertions.*
import munit.Location

sealed trait DebugStepAssert
final case class SingleStepAssert[T](step: DebugStep[T], assertion: T => Unit) extends DebugStepAssert
final case class ParallelStepsAsserts[T](steps: Seq[SingleStepAssert[T]]) extends DebugStepAssert

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
  def inParallel(steps: SingleStepAssert[Either[String, String]]*): ParallelStepsAsserts[Either[String, String]] =
    ParallelStepsAsserts(steps)

  def assertOnFrame(expectedSource: Path, expectedLine: Int)(frame: StackFrame)(implicit location: Location): Unit = {
    assertEquals(frame.source.path, expectedSource.toString)
    assertEquals(frame.line, expectedLine)
  }

  def assertOnFrame(expectedName: String)(frame: StackFrame): Unit =
    assertEquals(frame.name, expectedName)
}

object Breakpoint {
  def apply(line: Int)(implicit ctx: TestingContext): SingleStepAssert[StackFrame] =
    Breakpoint(ctx.mainSource, line)

  def apply(sourceFile: Path, line: Int): SingleStepAssert[StackFrame] = {
    val breakpoint = Breakpoint(sourceFile, line, None)
    SingleStepAssert(breakpoint, assertOnFrame(sourceFile, line))
  }

  def apply(line: Int, condition: String)(implicit ctx: TestingContext): SingleStepAssert[StackFrame] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, Some(condition))
    SingleStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line))
  }
}

object Logpoint {
  def apply(line: Int, logMessage: String, expected: String)(implicit
      ctx: TestingContext,
      loc: Location
  ): SingleStepAssert[String] = {
    val logpoint = Logpoint(ctx.mainSource, line, logMessage)
    SingleStepAssert(logpoint, output => assertEquals(output, expected))
  }
}

object StepIn {
  def line(line: Int)(implicit ctx: TestingContext, location: Location): SingleStepAssert[StackFrame] =
    SingleStepAssert(StepIn(), assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): SingleStepAssert[StackFrame] =
    SingleStepAssert(StepIn(), assertOnFrame(methodName))
}

object StepOut {
  def line(line: Int)(implicit ctx: TestingContext): SingleStepAssert[StackFrame] =
    SingleStepAssert(StepOut(), assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): SingleStepAssert[StackFrame] =
    SingleStepAssert(StepOut(), assertOnFrame(methodName))
}

object StepOver {
  def line(line: Int)(implicit ctx: TestingContext): SingleStepAssert[StackFrame] =
    SingleStepAssert(StepOver(), assertOnFrame(ctx.mainSource, line))

  def method(methodName: String): SingleStepAssert[StackFrame] =
    SingleStepAssert(StepOver(), assertOnFrame(methodName))
}

object Evaluation {
  def ignore(expression: String, expected: Any)(implicit
      ctx: TestingContext
  ): SingleStepAssert[Either[String, String]] =
    SingleStepAssert(Evaluation(expression), assertIgnore(expected.toString))

  def failed(expression: String, error: String): SingleStepAssert[Either[String, String]] =
    SingleStepAssert(Evaluation(expression), assertFailed(error))

  def failed(expression: String): SingleStepAssert[Either[String, String]] =
    SingleStepAssert(Evaluation(expression), resp => assertFailed(resp))

  def failedOrIgnore(expression: String, error: String, ignore: Boolean)(implicit
      ctx: TestingContext
  ): SingleStepAssert[Either[String, String]] = {
    SingleStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore(error) _ else assertFailed(error) _
    )
  }

  def failedOrIgnore(expression: String, ignore: Boolean)(assertion: String => Unit)(implicit ctx: TestingContext) = {
    SingleStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("failure") _ else assertFailed(assertion) _
    )
  }

  def success(expression: String, result: Any)(implicit
      ctx: TestingContext,
      location: Location
  ): SingleStepAssert[Either[String, String]] =
    new SingleStepAssert(Evaluation(expression), assertSuccess(result))

  def success(expression: String)(assertion: String => Unit): SingleStepAssert[Either[String, String]] = {
    new SingleStepAssert(Evaluation(expression), assertSuccess(assertion))
  }

  def successOrIgnore(expression: String, result: Any, ignore: Boolean)(implicit
      ctx: TestingContext
  ): SingleStepAssert[Either[String, String]] = {
    val assertion = if (ignore) assertIgnore(result.toString) _ else assertSuccess(result)(_)
    new SingleStepAssert(Evaluation(expression), assertion)
  }

  def successOrIgnore(expression: String, ignore: Boolean)(
      assertion: String => Unit
  )(implicit ctx: TestingContext): SingleStepAssert[Either[String, String]] = {
    new SingleStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("sucess") _ else assertSuccess(assertion)(_)
    )
  }

  private def assertFailed(response: Either[String, String]): Unit = {
    if (response.isLeft) println(s"\u001b[32mExpected failure\u001b[0m")
    else println(s"\u001b[31mUnexpected success, got ${response}\u001b[0m")
    assert(response.isLeft)
  }

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
    println(s"\u001b[33mTODO fix in ${ctx.scalaVersion}: expected ${expected}\u001b[0m")
  }

  private def assertSuccess(assertion: String => Unit)(response: Either[String, String]): Unit = {
    assert(clue(response).isRight)
    val result = response.toOption.get
    assertion(result)
  }

  private def assertSuccess(
      expectedResult: Any
  )(response: Either[String, String])(implicit ctx: TestingContext, location: Location): Unit = {
    if (clue(response).isLeft) println(s"\u001b[31mExpected success, got ${response}\u001b[0m")
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
  def apply(expected: String): SingleStepAssert[String] =
    apply(message => assertEquals(message, expected))

  def apply(assertion: String => Unit): SingleStepAssert[String] =
    new SingleStepAssert(Outputed(), assertion)
}

object NoStep {
  def apply(): SingleStepAssert[Nothing] =
    new SingleStepAssert[Nothing](new NoStep(), _ => ())
}
