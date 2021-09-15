package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._
import scala.util.Try
import scala.util.Failure
import scala.util.Success

/**
 * Objects created on the remote JVM can be garbage-collected at any time.
 * https://stackoverflow.com/questions/25793688/life-span-of-jdi-mirrors-of-objects-living-in-a-remote-jvm
 *
 * This can be prevented by wrapping every object reference into a [[Safe]]
 * instance. It calls `disableCollection` at construction and `enableCollection`
 * when the final result is retrieved.
 *
 * You can get the result out of a [[Safe]] instance by calling `getResult`.
 * Then the object references are not protected anymore and can be
 * normally garbage collected.
 */
class Safe[A] private (
    private val result: Try[A],
    private val dispose: () => Unit
) {
  def map[B](f: A => B): Safe[B] = new Safe(result.map(f), dispose)

  def flatMap[B](f: A => Safe[B]): Safe[B] = {
    result match {
      case Failure(exception) => new Safe(Failure(exception), dispose)
      case Success(a) =>
        val b = f(a)
        new Safe(b.result, () => { dispose(); b.dispose() })
    }
  }

  def getResult: Try[A] = {
    dispose()
    result
  }

  def withFilter(p: A => Boolean): Safe[A] = {
    new Safe(result.withFilter(p).map(identity), dispose)
  }
}

object Safe {
  def apply[A <: Value](f: => A): Safe[A] = {
    val result = Try(f)
    result match {
      case Success(value) =>
        value match {
          case obj: ObjectReference =>
            new Safe(
              Try(obj.disableCollection()).map(_ => value),
              () => Try(obj.enableCollection())
            )
          case _ => lift(value)
        }
      case Failure(exception) =>
        new Safe(Failure(exception), () => ())
    }
  }

  def lift[A](a: A): Safe[A] = {
    new Safe(Success(a), () => ())
  }

  def join[A, B](safeA: Safe[A], safeB: Safe[B]): Safe[(A, B)] = {
    safeA.flatMap(a => safeB.map(b => (a, b)))
  }
}
