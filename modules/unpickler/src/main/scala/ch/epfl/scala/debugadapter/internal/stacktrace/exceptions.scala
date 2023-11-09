package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary

case class AmbiguousException(symbol: binary.Symbol, candidates: Seq[BinarySymbol])
    extends Exception(s"Found ${candidates.size} matching symbols for ${symbol.name}")

case class NotFoundException(symbol: binary.Symbol) extends Exception(s"Cannot find binary symbol of $symbol")

case class IgnoredException(symbol: binary.Symbol, reason: String)
    extends Exception(s"Ignored $symbol because: $reason")

case class UnexpectedException(message: String) extends Exception(message)

def notFound(symbol: binary.Symbol): Nothing = throw NotFoundException(symbol)

def ambiguous(symbol: binary.Symbol, candidates: Seq[BinarySymbol]): Nothing =
  throw AmbiguousException(symbol, candidates)

def ignore(symbol: binary.Symbol, reason: String): Nothing = throw IgnoredException(symbol, reason)

def unexpected(message: String): Nothing = throw UnexpectedException(message)
