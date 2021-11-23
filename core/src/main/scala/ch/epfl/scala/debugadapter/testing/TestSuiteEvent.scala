package ch.epfl.scala.debugadapter.testing

import sbt.testing.{Event, Status}

import scala.collection.JavaConverters.*
import scala.collection.mutable
import ch.epfl.scala.debugadapter.DebuggeeListener

trait TestSuiteEventHandler {
  def handle(testSuiteEvent: TestSuiteEvent): Unit
}

object TestSuiteEventHandler {
  def formatError(
      testName: String,
      failureMessage: String,
      indentSize: Int
  ): String = {
    val indent = " " * indentSize
    (testName, failureMessage) match {
      case ("", failureMessage) => s"$indent* $failureMessage"
      case (testName, "") => s"$indent* $testName"
      case (testName, failureMessage) => s"$indent* $testName - $failureMessage"
    }
  }

  def summarizeResults(
      testSuiteResult: TestSuiteEvent.Results
  ): TestSuiteSummary = {
    val results = testSuiteResult.events.map { e =>
      val name = TestUtils.printSelector(e.selector).getOrElse("")
      val testResult: SingleTestSummary = e.status() match {
        case Status.Success =>
          SingleTestResult.Passed(name, testSuiteResult.duration)
        case Status.Failure =>
          val failedMsg =
            TestUtils.printThrowable(e.throwable()).getOrElse("")
          val formatted =
            TestSuiteEventHandler.formatError(
              name,
              failedMsg,
              indentSize = 0
            )
          SingleTestResult.Failed(name, testSuiteResult.duration, formatted)
        case _ =>
          SingleTestResult.Skipped(name)
      }
      testResult
    }.asJava
    val testResults = TestSuiteSummary(
      testSuiteResult.testSuite,
      testSuiteResult.duration,
      results
    )
    testResults
  }
}

sealed trait TestSuiteEvent
object TestSuiteEvent {
  case object Done extends TestSuiteEvent
  case class Error(message: String) extends TestSuiteEvent
  case class Warn(message: String) extends TestSuiteEvent
  case class Info(message: String) extends TestSuiteEvent
  case class Debug(message: String) extends TestSuiteEvent
  case class Trace(throwable: Throwable) extends TestSuiteEvent

  /** @param testSuite Class name of test suite */
  case class Results(testSuite: String, events: List[Event])
      extends TestSuiteEvent {

    // if no duration is available value is set to -1
    val duration = events.collect {
      case e if e.duration() > 0 => e.duration()
    }.sum
    def passed = events.count(_.status() == Status.Success)
    def skipped = events.count(_.status() == Status.Skipped)
    def failed = events.count(_.status() == Status.Failure)
    def canceled = events.count(_.status() == Status.Canceled)
    def ignored = events.count(_.status() == Status.Ignored)
    def pending = events.count(_.status() == Status.Pending)
    def errors = events.count(_.status() == Status.Error)
  }
}
