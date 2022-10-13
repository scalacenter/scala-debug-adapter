package ch.epfl.scala.debugadapter.testfmk

import java.nio.file.Path
import com.microsoft.java.debug.core.protocol.Types.StackFrame
import DebugStepAssert.*

final case class DebugStepAssert[T](step: DebugStep[T], assertion: T => Unit)

sealed trait DebugStep[+T]
final case class Breakpoint(sourceFile: Path, line: Int, condition: Option[String]) extends DebugStep[StackFrame]
final case class StepIn() extends DebugStep[StackFrame]
final case class StepOut() extends DebugStep[StackFrame]
final case class StepOver() extends DebugStep[StackFrame]
final case class Evaluation(expression: String) extends DebugStep[Either[String, String]]
final case class NoStep() extends DebugStep[Nothing]

object DebugStepAssert {
  def assertOnFrame(expectedSource: Path, expectedLine: Int)(frame: StackFrame): Unit = {
    val source = frame.source.path
    val line = frame.line
    utest.assert(source == expectedSource.toString)
    utest.assert(line == expectedLine)
  }

  def assertOnFrame(expectedName: String)(frame: StackFrame): Unit = {
    val name = frame.name
    utest.assert(name == expectedName)
  }
}

object Breakpoint {
  def apply(line: Int)(implicit debuggee: TestingDebuggee): DebugStepAssert[StackFrame] =
    Breakpoint(debuggee.mainSource, line)

  def apply(sourceFile: Path, line: Int): DebugStepAssert[StackFrame] = {
    val breakpoint = Breakpoint(sourceFile, line, None)
    DebugStepAssert(breakpoint, assertOnFrame(sourceFile, line))
  }

  def apply(line: Int, condition: String)(implicit debuggee: TestingDebuggee): DebugStepAssert[StackFrame] = {
    val breakpoint = Breakpoint(debuggee.mainSource, line, Some(condition))
    DebugStepAssert(breakpoint, assertOnFrame(debuggee.mainSource, line))
  }
}

object StepIn {
  def line(line: Int)(implicit debuggee: TestingDebuggee): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepIn(), assertOnFrame(debuggee.mainSource, line))

  def method(methodName: String): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepIn(), assertOnFrame(methodName))
}

object StepOut {
  def line(line: Int)(implicit debuggee: TestingDebuggee): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOut(), assertOnFrame(debuggee.mainSource, line))

  def method(methodName: String): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOut(), assertOnFrame(methodName))
}

object StepOver {
  def line(line: Int)(implicit debuggee: TestingDebuggee): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOver(), assertOnFrame(debuggee.mainSource, line))

  def method(methodName: String): DebugStepAssert[StackFrame] =
    DebugStepAssert(StepOver(), assertOnFrame(methodName))
}

object Evaluation {
  def ignore(expression: String, expected: Any)(implicit
      debuggee: TestingDebuggee
  ): DebugStepAssert[Either[String, String]] =
    new DebugStepAssert(Evaluation(expression), assertIgnore(expected.toString))

  def failed(expression: String, error: String): DebugStepAssert[Either[String, String]] =
    DebugStepAssert(Evaluation(expression), assertFailed(error))

  def failed(expression: String): DebugStepAssert[Either[String, String]] =
    DebugStepAssert(Evaluation(expression), resp => assertFailed(resp))

  def failedOrIgnore(expression: String, error: String, ignore: Boolean)(implicit
      debuggee: TestingDebuggee
  ): DebugStepAssert[Either[String, String]] = {
    new DebugStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore(error) _ else assertFailed(error) _
    )
  }

  def failedOrIgnore(expression: String, ignore: Boolean)(
      assertion: String => Unit
  )(implicit debuggee: TestingDebuggee) = {
    new DebugStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("failure") _ else assertFailed(assertion) _
    )
  }

  def success(expression: String, result: Any)(implicit
      debuggee: TestingDebuggee
  ): DebugStepAssert[Either[String, String]] =
    new DebugStepAssert(Evaluation(expression), assertSuccess(result))

  def success(expression: String)(assertion: String => Unit): DebugStepAssert[Either[String, String]] = {
    new DebugStepAssert(Evaluation(expression), assertSuccess(assertion))
  }

  def successOrIgnore(expression: String, result: Any, ignore: Boolean)(implicit
      debuggee: TestingDebuggee
  ): DebugStepAssert[Either[String, String]] = {
    val assertion = if (ignore) assertIgnore(result.toString) _ else assertSuccess(result)(_)
    new DebugStepAssert(Evaluation(expression), assertion)
  }

  def successOrIgnore(expression: String, ignore: Boolean)(
      assertion: String => Unit
  )(implicit debuggee: TestingDebuggee): DebugStepAssert[Either[String, String]] = {
    new DebugStepAssert(
      Evaluation(expression),
      if (ignore) assertIgnore("sucess") _ else assertSuccess(assertion)(_)
    )
  }

  private def assertFailed(response: Either[String, String]): Unit =
    utest.assert(response.isLeft)

  private def assertFailed(assertion: String => Unit)(response: Either[String, String]): Unit = {
    utest.assert(response.isLeft)
    val error = response.left.toOption.get
    assertion(error)
  }

  private def assertFailed(expectedError: String)(response: Either[String, String]): Unit = {
    utest.assert(response.isLeft)
    val error = response.left.toOption.get
    utest.assert(error.contains(expectedError))
  }

  private def assertIgnore(
      expected: String
  )(response: Either[String, String])(implicit debuggee: TestingDebuggee): Unit = {
    println(s"TODO fix in ${debuggee.scalaVersion}: expected $expected")
  }

  private def assertSuccess(assertion: String => Unit)(response: Either[String, String]): Unit = {
    utest.assert(response.isRight)
    val result = response.toOption.get
    assertion(result)
  }

  private def assertSuccess(
      expectedResult: Any
  )(response: Either[String, String])(implicit debuggee: TestingDebuggee): Unit = {
    utest.assert(response.isRight)
    val result = response.toOption.get
    expectedResult match {
      case expected: String =>
        utest.assert(result == '"' + expected + '"')
      case () =>
        if (debuggee.scalaVersion.isScala3) utest.assert(result.endsWith("\"()\""))
        else utest.assert(result == "<void value>")
      case expected @ (_: Boolean | _: Byte | _: Char | _: Int | _: Long | _: Short) =>
        utest.assert(result == expected.toString)
      case floating @ (_: Double | _: Float) =>
        val expected = String.format("%f", floating.toString().toDouble: java.lang.Double)
        utest.assert(result == expected)
      case expected =>
        // they have the same toString
        utest.assert(result.endsWith("\"" + expected + "\""))
    }
  }
}

object NoStep {
  def apply(): DebugStepAssert[Nothing] =
    new DebugStepAssert[Nothing](new NoStep(), (_: Nothing) => ())
}
