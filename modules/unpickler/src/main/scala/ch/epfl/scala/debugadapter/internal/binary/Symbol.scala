package ch.epfl.scala.debugadapter.internal.binary

import ch.epfl.scala.debugadapter.internal.stacktrace.NameTransformer

trait Symbol:
  def name: String
  def sourceLines: Seq[SourceLine]

  def unexpandedName: String =
    val expanded = ".+\\$\\$(_\\$)*(.+)".r
    name match
      case expanded(_, name) => name
      case _ => name

  def unexpandedDecodedName: String =
    NameTransformer.decode(unexpandedName)

  def decodedName: String =
    NameTransformer.decode(name)
