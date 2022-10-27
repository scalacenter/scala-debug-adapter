package ch.epfl.scala.debugadapter.internal

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import ch.epfl.scala.debugadapter.Logger

private[debugadapter] object TimeUtils {
  def logTime[T](logger: Logger, msg: String)(f: => T): T = {
    val (duration, result) = timed(f)
    logger.info(s"$msg in ${prettyPrint(duration)}")
    result
  }

  def timed[T](f: => T): (Duration, T) = {
    val start = System.currentTimeMillis
    val result = f
    val duration =
      Duration(System.currentTimeMillis - start, TimeUnit.MILLISECONDS)
    (duration, result)
  }

  def prettyPrint(duration: Duration): String = {
    def plural(n: Long, word: String): String =
      if (n == 1) s"1 $word" else s"$n ${word}s"

    duration match {
      case duration if duration.toSeconds == 0 =>
        plural(duration.toMillis, "millisecond")
      case duration if duration.toMinutes == 0 =>
        plural(duration.toSeconds, "second")
      case duration if duration.toHours == 0 =>
        plural(duration.toMinutes, "minute")
      case duration if duration.toDays == 0 => plural(duration.toHours, "hour")
      case duration if duration.toDays < 30 => plural(duration.toDays, "day")
      case duration if duration.toDays >= 30 && duration.toDays <= 365 =>
        plural(duration.toDays / 30, "month")
      case duration if duration.toDays > 365 =>
        plural(duration.toDays / 365, "year")
      case _ => duration.toString()
    }
  }
}
