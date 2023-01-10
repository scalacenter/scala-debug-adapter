package ch.epfl.scala.debugadapter.testing

import sbt.testing.{Event, Status}

sealed trait TestSuiteEvent
object TestSuiteEvent {
  case object Done extends TestSuiteEvent
  case class Error(message: String) extends TestSuiteEvent
  case class Warn(message: String) extends TestSuiteEvent
  case class Info(message: String) extends TestSuiteEvent
  case class Debug(message: String) extends TestSuiteEvent
  case class Trace(throwable: Throwable) extends TestSuiteEvent

  /** @param testSuite Class name of test suite */
  case class Results(testSuite: String, events: List[Event]) extends TestSuiteEvent {

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
