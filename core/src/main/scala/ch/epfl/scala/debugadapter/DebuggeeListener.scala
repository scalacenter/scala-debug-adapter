package ch.epfl.scala.debugadapter

import java.net.InetSocketAddress
import com.microsoft.java.debug.core.protocol.Events.DebugEvent

final class TestResultEvent private (
    val category: String,
    val data: Object
) extends DebugEvent("testResult")
object TestResultEvent {
  def apply(data: Object): TestResultEvent =
    new TestResultEvent("testResult", data)
}

final case class TestSuiteResult(
    suiteName: String,
    duration: Long,
    tests: java.util.List[SingleTestResult]
)
final case class SingleTestResult(
    status: String,
    testName: String,
    duration: Long,
    error: String
)
object SingleTestResult {
  def passed(testName: String, duration: Long): SingleTestResult =
    SingleTestResult("passed", testName, duration, error = null)

  def failed(
      testName: String,
      duration: Long,
      error: String
  ): SingleTestResult =
    SingleTestResult("failed", testName, duration, error)

  def skipped(testName: String): SingleTestResult =
    SingleTestResult("skipped", testName, 0, error = null)
}

trait DebuggeeListener {
  def onListening(address: InetSocketAddress): Unit
  def out(line: String): Unit
  def err(line: String): Unit
  def testResult(data: TestSuiteResult): Unit
}
