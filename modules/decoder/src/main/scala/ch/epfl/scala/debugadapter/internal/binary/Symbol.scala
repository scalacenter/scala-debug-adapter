package ch.epfl.scala.debugadapter.internal.binary

import ch.epfl.scala.debugadapter.internal.stacktrace.NameTransformer

trait Symbol:
  def name: String
  def sourceLines: Option[SourceLines]

  protected def showSpan: String =
    sourceLines.map(_.showSpan).getOrElse("")
