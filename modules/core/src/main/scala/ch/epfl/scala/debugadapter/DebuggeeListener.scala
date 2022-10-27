package ch.epfl.scala.debugadapter

import java.net.InetSocketAddress
import com.microsoft.java.debug.core.protocol.Events.DebugEvent
import ch.epfl.scala.debugadapter.testing.TestSuiteSummary

final class TestResultEvent private (
    val category: String,
    val data: TestSuiteSummary
) extends DebugEvent("testResult")
object TestResultEvent {
  def apply(data: TestSuiteSummary): TestResultEvent =
    new TestResultEvent("testResult", data)
}

trait DebuggeeListener {
  def onListening(address: InetSocketAddress): Unit
  def out(line: String): Unit
  def err(line: String): Unit
  def testResult(data: TestSuiteSummary): Unit
}
