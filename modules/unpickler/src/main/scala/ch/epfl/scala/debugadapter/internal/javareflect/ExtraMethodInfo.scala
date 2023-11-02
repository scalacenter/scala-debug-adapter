package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary.SourceLines
import ch.epfl.scala.debugadapter.internal.binary.Instruction

private case class ExtraMethodInfo(sourceLines: Option[SourceLines], instructions: Seq[Instruction])

private object ExtraMethodInfo:
  def empty: ExtraMethodInfo = ExtraMethodInfo(None, Seq.empty)
