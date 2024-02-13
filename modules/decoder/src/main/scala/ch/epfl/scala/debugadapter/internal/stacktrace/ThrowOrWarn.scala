package ch.epfl.scala.debugadapter.internal.stacktrace

import scala.util.control.NonFatal

object ThrowOrWarn:
  def printAndThrow = ThrowOrWarn(println, true)
  def justPrint = ThrowOrWarn(println, false)
  def ignore = ThrowOrWarn(_ => (), false)

class ThrowOrWarn(warnLogger: String => Unit, testMode: Boolean):
  def throwOrWarn(message: String): Unit =
    throwOrWarn(new Exception(message))

  def throwOrWarn(exception: Throwable): Unit =
    if testMode then throw exception
    else warnLogger(exception.getMessage)

  def tryOrNone[T](f: => T): Option[T] =
    try Some(f)
    catch
      case NonFatal(e) =>
        throwOrWarn(e)
        None

def tryOrNone[T](f: => T)(using self: ThrowOrWarn): Option[T] =
  self.tryOrNone(f)

def throwOrWarn(exception: Throwable)(using self: ThrowOrWarn): Unit =
  self.throwOrWarn(exception)

def throwOrWarn(message: String)(using self: ThrowOrWarn): Unit =
  self.throwOrWarn(message)
