package dap.internal

import java.util.{Timer, TimerTask}
import java.util.concurrent.TimeoutException
import scala.concurrent.Promise
import scala.concurrent.duration.Duration

private[dap] object Scheduler {
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
