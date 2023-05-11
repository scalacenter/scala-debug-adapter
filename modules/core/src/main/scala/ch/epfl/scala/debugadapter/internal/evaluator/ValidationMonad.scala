package ch.epfl.scala.debugadapter.internal.evaluator

import java.util.NoSuchElementException

sealed abstract class Validation[+A]() {
  def isValid: Boolean
  def canContinue: Boolean
  def isInvalid: Boolean = !isValid

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

  def toOption: Option[A]
}

final case class Valid[+A](value: A) extends Validation[A]() {
  override val isValid: Boolean = true
  override val canContinue: Boolean = true
  override def map[B](f: A => B): Validation[B] = Validation(f(value))
  override def flatMap[B](f: A => Validation[B]): Validation[B] =
    f(value).map(Validation(_)).flatten
  override def get = value
  override def getOrElse[B >: A](f: => B): B = value
  override def orElse[B >: A](f: => Validation[B]): Validation[B] = this

  override def toOption: Option[A] = Some(value)
}

sealed abstract class Invalid(val exception: Exception) extends Validation[Nothing]() {
  override val isValid: Boolean = false
  override def map[B](f: Nothing => B): Validation[B] = this
  override def flatMap[B](f: Nothing => Validation[B]): Validation[B] = this
  override def get = throw exception
  override def getOrElse[B >: Nothing](f: => B): B = f

  override def toOption: Option[Nothing] = None
}

final case class Recoverable(message: String) extends Invalid(new NoSuchElementException(message)) {
  override val canContinue: Boolean = true
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = f
}

final case class Unrecoverable(e: Exception) extends Invalid(e) {
  override val canContinue: Boolean = false
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
