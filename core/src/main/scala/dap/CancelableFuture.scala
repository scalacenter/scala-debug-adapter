package dap

import scala.concurrent.Future

/**
  * As soon as cancel is called, future is supposed to return
  */
trait CancelableFuture[T] {
  def future: Future[T]
  def cancel(): Unit
}
