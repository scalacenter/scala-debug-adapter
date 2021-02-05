package dap

import java.nio.file.Path
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class MockDebuggeeRunner() extends DebuggeeRunner {
  var currentProcess: MockCancelableFuture = _

  override def name: String = "mock"

  override def run(callbacks: DebuggeeLogger): CancelableFuture[Unit] = {
    if (currentProcess != null) {
      // wait for the current process to finish
      Await.result(currentProcess.future(), 500 millis)
    }
    currentProcess = new MockCancelableFuture()
    currentProcess
  }
  
  override def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path] = List.empty
}

class MockCancelableFuture() extends CancelableFuture[Unit] {
  val stopped: Promise[Unit] = Promise[Unit]()
  override def future(): Future[Unit] = stopped.future
  override def cancel(): Unit = stopped.trySuccess(())
}