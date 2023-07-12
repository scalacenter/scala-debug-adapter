package ch.epfl.scala.debugadapter.internal.evaluator

import java.util.NoSuchElementException
import scala.util.Try
import scala.util.Success
import scala.util.Failure

import com.sun.jdi.VMDisconnectedException
import com.sun.jdi.ObjectCollectedException
import com.sun.jdi.InvalidStackFrameException
import com.sun.jdi.AbsentInformationException
import com.sun.jdi.InvocationException
import com.sun.jdi.VMOutOfMemoryException
import ch.epfl.scala.debugadapter.Logger

sealed abstract class Validation[+A] {
  def isValid: Boolean
  def isInvalid: Boolean = !isValid
  def isEmpty: Boolean = isInvalid

  def filter(p: A => Boolean, runtimeFatal: Boolean = false): Validation[A]
  def filterNot(p: A => Boolean, runtimeFatal: Boolean = false): Validation[A] = filter(!p(_), runtimeFatal)
  def withFilter(p: A => Boolean): Validation[A] = filter(p)

  def map[B](f: A => B)(implicit logger: Logger): Validation[B]
  def flatMap[B](f: A => Validation[B])(implicit logger: Logger): Validation[B]
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
  override def map[B](f: A => B)(implicit logger: Logger): Validation[B] = Validation(f(value))
  override def flatMap[B](f: A => Validation[B])(implicit logger: Logger): Validation[B] =
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
  override def map[B](f: Nothing => B)(implicit logger: Logger): Validation[B] = this
  override def flatMap[B](f: Nothing => Validation[B])(implicit logger: Logger): Validation[B] = this
  override def get = throw exception
  override def getOrElse[B >: Nothing](f: => B): B = f

  override def filter(p: Nothing => Boolean, fatal: Boolean): Validation[Nothing] = this

  override def toOption: Option[Nothing] = None
  override def toTry: Try[Nothing] = Failure(exception)
}

final case class Recoverable(override val exception: Exception) extends Invalid(exception) {
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = f
}

sealed abstract class Unrecoverable(override val exception: Exception) extends Invalid(exception) {
  override def orElse[B >: Nothing](f: => Validation[B]): Validation[B] = this
  override def transform[B](f: Validation[Nothing] => Validation[B]): Validation[B] = this
}

final case class Fatal(override val exception: Exception) extends Unrecoverable(exception)
final case class CompilerRecoverable(override val exception: Exception) extends Unrecoverable(exception)

object Validation {
  private def handler(t: Throwable)(implicit logger: Logger): Invalid = t match {
    case e @ (_: VMDisconnectedException | _: ObjectCollectedException) => Fatal(e)
    case e @ (_: InvalidStackFrameException | _: AbsentInformationException) => Fatal(e)
    case e @ (_: InvocationException | _: VMOutOfMemoryException) => Fatal(e)
    case e: Exception =>
      logger.warn(s"\u001b[35mUnexpected error while validating: ${e}\u001b[0m")
      CompilerRecoverable(e)
  }

  def apply[A](input: => A)(implicit logger: Logger): Validation[A] = {
    try {
      val value = input
      if (value == null) Recoverable("Found null value, expected non-null value")
      else Valid(value)
    } catch {
      case t: Throwable => handler(t)
    }
  }

  def fromOption[A](value: => Option[A]): Validation[A] = {
    value match {
      case Some(value) => Valid(value)
      case None => Recoverable("Empty option")
    }
  }

  def fromTry[A](value: => scala.util.Try[A])(implicit logger: Logger): Validation[A] = {
    value match {
      case scala.util.Success(value) => Valid(value)
      case scala.util.Failure(t) => handler(t)
    }
  }

}

object Invalid {
  def unapply(invalid: Invalid): Option[Exception] = Some(invalid.exception)
}

object Recoverable {
  def apply(s: String) = new Recoverable(new NoSuchElementException(s))
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
