package ch.epfl.scala.debugadapter.testfmk

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.language.postfixOps
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.DebuggeeListener
import ch.epfl.scala.debugadapter.CancelableFuture
import ch.epfl.scala.debugadapter.Module
import ch.epfl.scala.debugadapter.Library
import ch.epfl.scala.debugadapter.UnmanagedEntry
import ch.epfl.scala.debugadapter.JavaRuntime
import io.reactivex.subjects.PublishSubject
import io.reactivex.Observable

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

  override val classesToUpdate: Observable[Seq[String]] = PublishSubject.create[Seq[String]]()
}

class MockCancelableFuture() extends CancelableFuture[Unit] {
  val stopped: Promise[Unit] = Promise[Unit]()
  override def future: Future[Unit] = stopped.future
  override def cancel(): Unit = stopped.trySuccess(())
}
