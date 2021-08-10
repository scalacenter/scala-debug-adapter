package ch.epfl.scala.debugadapter

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.language.postfixOps

class MockDebuggeeRunner() extends DebuggeeRunner {
  var currentProcess: MockCancelableFuture = _

  override def name: String = "mock"

  override def run(listener: DebuggeeListener): CancelableFuture[Unit] = {
    if (currentProcess != null) {
      // wait for the current process to finish
      Await.result(currentProcess.future, 500 millis)
    }
    currentProcess = new MockCancelableFuture()
    currentProcess
  }

  override def classPathEntries: Seq[ClassPathEntry] = Seq.empty
}

class MockCancelableFuture() extends CancelableFuture[Unit] {
  val stopped: Promise[Unit] = Promise[Unit]()
  override def future: Future[Unit] = stopped.future
  override def cancel(): Unit = stopped.trySuccess(())
}