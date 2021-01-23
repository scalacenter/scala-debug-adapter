/*
 * Copyright 2020 Scala Debug Adapter contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dap

import java.util.{Timer, TimerTask}
import java.util.concurrent.TimeoutException
import scala.concurrent.Promise
import scala.concurrent.duration.Duration

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
  }
}