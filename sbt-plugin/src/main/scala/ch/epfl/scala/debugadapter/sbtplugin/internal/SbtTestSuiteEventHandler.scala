package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter.DebuggeeListener
import ch.epfl.scala.debugadapter.testing.TestSuiteEventHandler
import ch.epfl.scala.debugadapter.testing.TestSuiteEvent

/**
 * Extracts information about tests execution and send it to the DebuggeeListener.
 * Then DebugeeListener forwards it to the DAP client.
 */
class SbtTestSuiteEventHandler(listener: DebuggeeListener) extends TestSuiteEventHandler {

  def handle(event: TestSuiteEvent): Unit =
    event match {
      case TestSuiteEvent.Info(s) => listener.out(s)
      case TestSuiteEvent.Warn(s) => listener.out(s)
      case TestSuiteEvent.Error(s) => listener.err(s)
      case results: TestSuiteEvent.Results =>
        val testResults = TestSuiteEventHandler.summarizeResults(results)
        listener.testResult(testResults)
      case _ => ()
    }
}
