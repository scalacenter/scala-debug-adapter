package ch.epfl.scala.debugadapter.internal.binary

trait Symbol:
  def name: String
  def sourceLines: Seq[SourceLine]
