package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi._

import scala.util.{Failure, Success, Try}

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
class Safe[+A] private (
    private val result: Try[A],
    private val dispose: () => Unit
) {
  def extract[B](f: A => B): Try[B] = result.map(f)
  def extract: Try[A] = result

  def map[B](f: A => B): Safe[B] = new Safe(result.map(f), dispose)

  def flatMap[B](f: A => Safe[B]): Safe[B] = {
    result match {
      case Failure(exception) => new Safe(Failure(exception), dispose)
      case Success(a) =>
        val b = f(a)
        new Safe(b.result, () => { dispose(); b.dispose() })
    }
  }

  def orElse[B >: A](alternative: => Safe[B]): Safe[B] =
    result match {
      case Failure(_) => alternative
      case _ => this
    }

  def getResult: Try[A] = {
    dispose()
    result
  }

  def withFilter(p: A => Boolean): Safe[A] = {
    new Safe(result.withFilter(p).map(identity), dispose)
  }

  def withFilterNot(p: A => Boolean): Safe[A] = withFilter(!p(_))

  def recover[B >: A](f: PartialFunction[Throwable, B]): Safe[B] = {
    new Safe(result.recover(f), dispose)
  }

  def recoverWith[B >: A](f: PartialFunction[Throwable, Safe[B]]): Safe[B] = {
    result match {
      case Failure(exception) if f.isDefinedAt(exception) =>
        val b = f(exception)
        new Safe(
          b.result,
          () => {
            dispose(); b.dispose()
          }
        )
      case _ => new Safe(result, dispose)
    }
  }
}

object Safe {
  def apply[A](f: => A): Safe[A] = {
    val result = Try(f)
    apply(result)
  }

  def apply[A](a: Try[A]): Safe[A] = {
    a match {
      case Success(value) =>
        value match {
          case obj: ObjectReference =>
            new Safe(
              Try(obj.disableCollection()).map(_ => value),
              () => Try(obj.enableCollection())
            )
          case _ => new Safe(Success(value), () => ())
        }
      case Failure(exception) =>
        new Safe(Failure(exception), () => ())
    }
  }

  def unapply[A](safe: Safe[A]): Option[Try[A]] = Some(safe.result)

  def join[A, B](safeA: Safe[A], safeB: Safe[B]): Safe[(A, B)] = {
    safeA.flatMap(a => safeB.map(b => (a, b)))
  }

  def failed(exception: Throwable): Safe[Nothing] = {
    new Safe(Failure(exception), () => ())
  }

  def successful[A](a: A): Safe[A] = {
    new Safe(Success(a), () => ())
  }

  def failed(message: String): Safe[Nothing] = {
    failed(new Exception(message))
  }
}
