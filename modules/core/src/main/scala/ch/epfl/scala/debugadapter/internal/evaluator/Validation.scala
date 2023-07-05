package ch.epfl.scala.debugadapter.internal.evaluator

import java.util.NoSuchElementException
import scala.util.Try
import scala.util.Success
import scala.util.Failure

sealed abstract class Validation[+A] {
  def isValid: Boolean
  def isInvalid: Boolean = !isValid
  def isEmpty: Boolean = isInvalid

  def filter(p: A => Boolean, runtimeFatal: Boolean = false): Validation[A]
  def filterNot(p: A => Boolean, runtimeFatal: Boolean = false): Validation[A] = filter(!p(_), runtimeFatal)

  def map[B](f: A => B): Validation[B]
  def flatMap[B](f: A => Validation[B]): Validation[B]
  def flatten[B](implicit ev: A <:< Validation[B]): Validation[B] = this match {
    case Valid(value) => ev(this.get)
    case Recoverable(message) => Recoverable(message)
    case Fatal(e) => Fatal(e)
    case CompilerRecoverable(message) => CompilerRecoverable(message)
  }

  def transform[B](f: Validation[A] => Validation[B]): Validation[B] = f(this)

  def get: A
  def getOrElse[B >: A](f: => B): B
  def orElse[B >: A](f: => Validation[B]): Validation[B]

  def toOption: Option[A]
  def toTry: Try[A]
}

final case class Valid[+A](value: A) extends Validation[A]() {
  override val isValid: Boolean = true
  override def map[B](f: A => B): Validation[B] = Validation(f(value))
  override def flatMap[B](f: A => Validation[B]): Validation[B] =
    f(value).map(Validation(_)).flatten
  override def get = value
  override def getOrElse[B >: A](f: => B): B = value
  override def orElse[B >: A](f: => Validation[B]): Validation[B] = this

  override def filter(p: A => Boolean, fatal: Boolean): Validation[A] =
    if (p(value)) this
    else if (fatal) CompilerRecoverable(s"Predicate does not hold for $value")
    else Recoverable(s"Predicate does not hold for $value")

  override def toOption: Option[A] = Some(value)
  override def toTry: Try[A] = Success(value)
}

sealed abstract class Invalid(val exception: Exception) extends Validation[Nothing]() {
  override val isValid: Boolean = false
  override def map[B](f: Nothing => B): Validation[B] = this
  override def flatMap[B](f: Nothing => Validation[B]): Validation[B] = this
  override def get = throw exception
  override def getOrElse[B >: Nothing](f: => B): B = f

  override def filter(p: Nothing => Boolean, fatal: Boolean): Validation[Nothing] = this

  override def toOption: Option[Nothing] = None
  override def toTry: Try[Nothing] = Failure(exception)
}

final case class Recoverable(message: String) extends Invalid(new NoSuchElementException(message)) {
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = f
}

sealed abstract class Unrecoverable(override val exception: Exception) extends Invalid(exception) {
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = this
}

final case class Fatal(override val exception: Exception) extends Unrecoverable(exception)
final case class CompilerRecoverable(override val exception: Exception) extends Unrecoverable(exception)

object Validation {
  def apply[A](input: => A): Validation[A] = {
    try {
      val value = input
      if (value == null) Recoverable("Found null value, expected non-null value")
      else Valid(value)
    } catch {
      case t: Throwable => CompilerRecoverable(new Exception(t))
    }
  }

  def fromOption[A](value: => Option[A]): Validation[A] = {
    value match {
      case Some(value) => Valid(value)
      case None => Recoverable("Empty option")
    }
  }

  def fromTry[A](value: => scala.util.Try[A]): Validation[A] = {
    value match {
      case scala.util.Success(value) => Valid(value)
      case scala.util.Failure(t) => CompilerRecoverable(new Exception(t))
    }
  }

}

object Invalid {
  def unapply(invalid: Invalid): Option[Exception] = Some(invalid.exception)
}

object Recoverable {
  def apply(t: Throwable): Recoverable = Recoverable(t.getMessage())
}

object Fatal {
  def apply(t: Throwable): Unrecoverable = new Fatal(new Exception(t))
  def apply(str: String): Unrecoverable = new Fatal(new Exception(str))
}

object CompilerRecoverable {
  def apply(t: Throwable): Unrecoverable = CompilerRecoverable(
    new Exception(s"Can't validate at runtime, recovering with compiler: $t")
  )
  def apply(str: String): Unrecoverable = CompilerRecoverable(
    new Exception(s"Can't validate at runtime, recovering with compiler: $str")
  )
}
