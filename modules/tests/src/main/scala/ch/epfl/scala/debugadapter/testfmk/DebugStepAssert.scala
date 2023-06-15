package ch.epfl.scala.debugadapter.testfmk

import com.microsoft.java.debug.core.protocol.Types.StackFrame
import munit.Assertions.*
import munit.Location

import java.nio.file.Path
import scala.Console.*

import DebugStepAssert.*

sealed trait DebugStepAssert
final case class SingleStepAssert[T](step: DebugStep[T], assertion: T => Unit) extends DebugStepAssert
final case class ParallelStepsAsserts[T](steps: Seq[SingleStepAssert[T]]) extends DebugStepAssert

sealed trait DebugStep[+T]
final case class Breakpoint(sourceFile: Path, line: Int, condition: Option[String]) extends DebugStep[List[StackFrame]]
final case class Logpoint(sourceFile: Path, line: Int, logMessage: String) extends DebugStep[String]
final case class StepIn() extends DebugStep[List[StackFrame]]
final case class StepOut() extends DebugStep[List[StackFrame]]
final case class StepOver() extends DebugStep[List[StackFrame]]
final case class Evaluation(expression: String) extends DebugStep[Either[String, String]]
final case class Outputed() extends DebugStep[String]
final case class NoStep() extends DebugStep[Nothing]

final case class ObjectRef(clsName: String)

object DebugStepAssert {
  def inParallel(steps: SingleStepAssert[Either[String, String]]*): ParallelStepsAsserts[Either[String, String]] =
    ParallelStepsAsserts(steps)

  def assertOnFrame(expectedSource: Path, expectedLine: Int, expectedStackTrace: Option[List[String]])(
      frames: List[StackFrame]
  )(implicit location: Location): Unit = {
    assertEquals(frames.head.source.path, expectedSource.toString)
    assertEquals(frames.head.line, expectedLine)
    expectedStackTrace match {
      case None => {}
      case Some(expectedStackTrace) => {
        assertEquals(expectedStackTrace, frames.map(frame => frame.name))

      }

    }

  }

  def assertOnFrame(expectedName: String)(frames: List[StackFrame])(implicit loc: Location): Unit =
    assertEquals(frames.head.name, expectedName)
}

object Breakpoint {
  def apply(line: Int)(implicit ctx: TestingContext): SingleStepAssert[List[StackFrame]] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, None)
    SingleStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line, None))
  }

  def apply(sourceFile: Path, line: Int): SingleStepAssert[List[StackFrame]] = {
    val breakpoint = Breakpoint(sourceFile, line, None)
    SingleStepAssert(breakpoint, assertOnFrame(sourceFile, line, None))
  }
  def apply(line: Int, expectedStackTrace: List[String])(implicit
      ctx: TestingContext
  ): SingleStepAssert[List[StackFrame]] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, None)
    SingleStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line, Some(expectedStackTrace)))
  }

  def apply(line: Int, condition: String)(implicit ctx: TestingContext): SingleStepAssert[List[StackFrame]] = {
    val breakpoint = Breakpoint(ctx.mainSource, line, Some(condition))
    SingleStepAssert(breakpoint, assertOnFrame(ctx.mainSource, line, None))
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
  def line(line: Int)(implicit ctx: TestingContext, location: Location): SingleStepAssert[List[StackFrame]] =
    SingleStepAssert(StepIn(), assertOnFrame(ctx.mainSource, line, None))

  def method(methodName: String)(implicit loc: Location): SingleStepAssert[List[StackFrame]] =
    SingleStepAssert(StepIn(), assertOnFrame(methodName))
}

object StepOut {
  def line(line: Int)(implicit ctx: TestingContext): SingleStepAssert[List[StackFrame]] =
    SingleStepAssert(StepOut(), assertOnFrame(ctx.mainSource, line, None))

  def method(methodName: String): SingleStepAssert[List[StackFrame]] =
    SingleStepAssert(StepOut(), assertOnFrame(methodName))
}

object StepOver {
  def line(line: Int)(implicit ctx: TestingContext): SingleStepAssert[List[StackFrame]] =
    SingleStepAssert(StepOver(), assertOnFrame(ctx.mainSource, line, None))

  def method(methodName: String): SingleStepAssert[List[StackFrame]] =
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

  private def assertFailed(response: Either[String, String]): Unit =
    assert(response.isLeft, clue = s"Expected error, got ${response.right.toOption.get}$RESET")

  private def assertFailed(assertion: String => Unit)(response: Either[String, String]): Unit = {
    assertFailed(response)
    val error = response.left.toOption.get
    assertion(error)
  }

  private def assertFailed(expectedError: String)(response: Either[String, String]): Unit =
    assertFailed(error => assert(clue(error).contains(clue(expectedError))))(response)

  private def assertIgnore(
      expected: String
  )(response: Either[String, String])(implicit ctx: TestingContext): Unit = {
    println(s"${YELLOW}TODO fix in ${ctx.scalaVersion}: expected $expected$RESET")
  }

  private def assertSuccess(assertion: String => Unit)(response: Either[String, String]): Unit = {
    assert(clue(response).isRight)
    val result = response.toOption.get
    assertion(result)
  }

  private def assertSuccess(
      expectedResult: Any
  )(response: Either[String, String])(implicit ctx: TestingContext, location: Location): Unit = {
    if (clue(response).isLeft) println(s"${RED}Expected success, got ${response.left}${RESET}")
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
