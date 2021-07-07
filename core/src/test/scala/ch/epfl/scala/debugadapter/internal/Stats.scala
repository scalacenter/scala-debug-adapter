package ch.epfl.scala.debugadapter.internal

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

object Stats {
  def timed[T](f: => T): (Duration, T) = {
    val start = System.currentTimeMillis()
    val result = f
    val duration = Duration(System.currentTimeMillis - start, TimeUnit.MILLISECONDS)
    (duration, result)
  }
}
