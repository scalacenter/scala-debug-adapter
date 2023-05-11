package ch.epfl.scala.debugadapter.internal.evaluator

import java.util.NoSuchElementException
import scala.util.Try
import scala.util.Success
import scala.util.Failure

sealed abstract class Validation[+A] {
  def isValid: Boolean
  def isInvalid: Boolean = !isValid
  def isEmpty: Boolean = isInvalid

  def filter(p: A => Boolean): Validation[A] =
    if (isValid && p(get)) this
    else Recoverable(s"Predicate does not hold for $get")

  def map[B](f: A => B): Validation[B]
  def flatMap[B](f: A => Validation[B]): Validation[B]
  def flatten[B](implicit ev: A <:< Validation[B]): Validation[B] = this match {
    case Valid(value) => ev(this.get)
    case Recoverable(message) => Recoverable(message)
    case Unrecoverable(e) => Unrecoverable(e)
  }

  def get: A
  def getOrElse[B >: A](f: => B): B
  def orElse[B >: A](f: => Validation[B]): Validation[B]

  def recoverWith[B >: A](f: => Validation[B]): Validation[B]

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

  override def recoverWith[B >: A](f: => Validation[B]): Validation[B] = this

  override def toOption: Option[A] = Some(value)
  override def toTry: Try[A] = Success(value)
}

sealed abstract class Invalid(val exception: Exception) extends Validation[Nothing]() {
  override val isValid: Boolean = false
  override def map[B](f: Nothing => B): Validation[B] = this
  override def flatMap[B](f: Nothing => Validation[B]): Validation[B] = this
  override def get = throw exception
  override def getOrElse[B >: Nothing](f: => B): B = f

  override def recoverWith[B >: Nothing](f: => Validation[B]): Validation[B] = f

  override def toOption: Option[Nothing] = None
  override def toTry: Try[Nothing] = Failure(exception)
}

final case class Recoverable(message: String) extends Invalid(new NoSuchElementException(message)) {
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = f
}

final case class Unrecoverable(e: Exception) extends Invalid(e) {
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = this
}

object Validation {
  def apply[A](input: => A): Validation[A] = {
    try {
      val value = input
      if (value == null) Recoverable("Found null value, expected non-null value")
      else Valid(value)
    } catch {
      case t: Throwable => Unrecoverable(new Exception(t))
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
      case scala.util.Failure(t) => Unrecoverable(new Exception(t))
    }
  }

}

object Invalid {
  def unapply(invalid: Invalid): Option[Exception] = Some(invalid.exception)
}

object Unrecoverable {
  def apply(t: Throwable): Unrecoverable = Unrecoverable(new Exception(t))
  def apply(str: String): Unrecoverable = Unrecoverable(new Exception(str))
}
