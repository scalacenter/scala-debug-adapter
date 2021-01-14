package dap

import java.util.Timer
import scala.concurrent.Promise
import scala.concurrent.duration.Duration
import java.util.TimerTask
import java.util.concurrent.TimeoutException

object TimeoutScheduler {
  private val timer = new Timer("DAP Timeout Scheduler", true)

  implicit class TimeoutPromise[T](promise: Promise[T]) {
    def timeout(duration: Duration): Promise[T] = {
      val task = new TimerTask {
        def run(): Unit =
          promise.tryFailure(new TimeoutException(s"Operation timed out after $duration"))
      }
      timer.schedule(task, duration.toMillis)
      promise
    }

    def timeoutTo(duration: Duration, fallback: () => T) = {
      val task = new TimerTask {
        def run(): Unit = fallback()
      }
      timer.schedule(task, duration.toMillis)
      promise
    }
  }
}