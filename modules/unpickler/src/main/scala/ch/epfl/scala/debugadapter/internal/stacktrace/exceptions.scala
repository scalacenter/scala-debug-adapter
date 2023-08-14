package ch.epfl.scala.debugadapter.internal.stacktrace

case class AmbiguousException(m: String) extends Exception
case class NotFoundException(m: String) extends Exception
