package ch.epfl.scala.debugadapter.testing

final case class TestSuiteSummary(
    suiteName: String,
    duration: Long,
    tests: java.util.List[SingleTestSummary]
)

sealed trait SingleTestSummary
object SingleTestResult {
  final class Passed private (
      val kind: String,
      val testName: String,
      val duration: Long
  ) extends SingleTestSummary
  object Passed {
    def apply(testName: String, duration: Long): Passed =
      new Passed("passed", testName, duration)
  }

  final class Skipped private (
      val kind: String,
      val testName: String
  ) extends SingleTestSummary
  object Skipped {
    def apply(testName: String): Skipped = new Skipped("skipped", testName)
  }

  final class Failed private (
      val kind: String,
      val testName: String,
      val duration: Long,
      val error: String
  ) extends SingleTestSummary
  object Failed {
    def apply(testName: String, duration: Long, error: String): Failed =
      new Failed("failed", testName, duration, error)
  }
}
