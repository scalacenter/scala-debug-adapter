package ch.epfl.scala.debugadapter

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.language.postfixOps

class MockDebuggee extends Debuggee {
  val scalaVersion: ScalaVersion = ScalaVersion.`2.12`
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

  override def modules: Seq[Module] = Seq.empty
  override def libraries: Seq[Library] = Seq.empty
  override def unmanagedEntries: Seq[UnmanagedEntry] = Seq.empty

  override def javaRuntime: Option[JavaRuntime] = None
}

class MockCancelableFuture() extends CancelableFuture[Unit] {
  val stopped: Promise[Unit] = Promise[Unit]()
  override def future: Future[Unit] = stopped.future
  override def cancel(): Unit = stopped.trySuccess(())
}
