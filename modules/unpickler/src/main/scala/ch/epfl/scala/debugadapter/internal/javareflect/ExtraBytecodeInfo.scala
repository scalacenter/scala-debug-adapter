package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary.SourceLine
import ch.epfl.scala.debugadapter.internal.binary.Instruction

private case class ExtraBytecodeInfo(sourceLines: Seq[SourceLine], instructions: Seq[Instruction])

private object ExtraBytecodeInfo:
  def empty: ExtraBytecodeInfo = ExtraBytecodeInfo(Seq.empty, Seq.empty)
