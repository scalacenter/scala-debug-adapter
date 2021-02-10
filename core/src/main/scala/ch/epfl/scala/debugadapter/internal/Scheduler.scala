package ch.epfl.scala.debugadapter.internal

import java.util.concurrent.TimeoutException
import java.util.{Timer, TimerTask}
import scala.concurrent.Promise
import scala.concurrent.duration.Duration

private[debugadapter] object Scheduler {
  private val timer = new Timer("DAP Timeout Scheduler", true)

  def timeout[T](promise: Promise[T], duration: Duration): Promise[T] = {
    val task = new TimerTask {
      def run(): Unit =
        promise.tryFailure(new TimeoutException(s"Operation timed out after $duration"))
    }
    timer.schedule(task, duration.toMillis)
    promise
  }
}
