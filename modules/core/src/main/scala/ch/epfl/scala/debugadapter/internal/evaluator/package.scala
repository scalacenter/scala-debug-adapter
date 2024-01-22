package ch.epfl.scala.debugadapter.internal

import scala.util.Try
import scala.util.Failure
import scala.util.Success
import ch.epfl.scala.debugadapter.Logger

package object evaluator {
  implicit class SafeSeq[A](seq: Seq[Safe[A]]) {
    def traverse: Safe[Seq[A]] = {
      seq.foldRight(Safe(Seq.empty[A])) { (safeHead, safeTail) =>
        safeTail.flatMap(tail => safeHead.map(head => head +: tail))
      }
    }
  }

  implicit class SafeOption[A](opt: Option[Safe[A]]) {
    def traverse: Safe[Option[A]] =
      opt.map(s => s.map(Option.apply)).getOrElse(Safe(None))
  }

  implicit class TryToSafe[A](t: Try[A]) {
    def toSafe: Safe[A] = Safe(t)
    def toSeq: Seq[A] = t match {
      case Failure(exception) => Seq()
      case Success(value) => Seq(value)
    }
  }

  implicit class ValidationSeq[A](seq: Seq[Validation[A]])(implicit logger: Logger) {
    def traverse: Validation[Seq[A]] = {
      seq.foldRight(Validation(Seq.empty[A])) { (safeHead, safeTail) =>
        safeTail.flatMap(tail => safeHead.map(head => head +: tail))
      }
    }
  }
}
