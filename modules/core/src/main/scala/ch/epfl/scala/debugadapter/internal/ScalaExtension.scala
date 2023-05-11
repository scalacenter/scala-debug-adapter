package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Logger
import scala.util.Success
import scala.util.Failure
import scala.util.Try

private[debugadapter] object ScalaExtension {
  implicit class TryExtension[T](x: Try[T]) {
    def warnFailure(logger: Logger, message: String): Option[T] = x match {
      case Success(value) => Some(value)
      case Failure(e) =>
        logger.warn(s"$message: ${e.getClass.getSimpleName} ${e.getMessage}")
        None
    }
  }

  implicit class OptionExtension[T](opt: Option[T]) {
    def toTry(message: String): Try[T] = {
      opt match {
        case Some(value) => Success(value)
        case None => Failure(new Exception(message))
      }
    }
  }

  implicit class TrySeq[A](seq: Seq[Try[A]]) {
    def traverse: Try[Seq[A]] = {
      seq.foldRight(Try(Seq.empty[A])) { (safeHead, safeTail) =>
        safeTail.flatMap(tail => safeHead.map(head => head +: tail))
      }
    }
  }
}
