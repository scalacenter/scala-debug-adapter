package ch.epfl.scala.debugadapter

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.language.postfixOps

class MockDebuggeeRunner() extends DebuggeeRunner {
  val scalaVersion: String = "2.12.15"
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
  override def javaRuntime: Option[JavaRuntime] = None
  override def evaluationClassLoader: Option[ClassLoader] = Some(
    getClass.getClassLoader
  )
  override def stepFilterClassLoader: Option[ClassLoader] = Some(
    getClass.getClassLoader
  )
}

class MockCancelableFuture() extends CancelableFuture[Unit] {
  val stopped: Promise[Unit] = Promise[Unit]()
  override def future: Future[Unit] = stopped.future
  override def cancel(): Unit = stopped.trySuccess(())
}
