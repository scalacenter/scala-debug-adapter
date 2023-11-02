package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary.SourceLines
import ch.epfl.scala.debugadapter.internal.binary.MethodSig

private final case class ExtraClassInfo(sourceLines: Option[SourceLines], methodsInfo: Map[MethodSig, ExtraMethodInfo]):
  def getMethodInfo(sig: MethodSig): ExtraMethodInfo = methodsInfo.getOrElse(sig, ExtraMethodInfo.empty)

private object ExtraClassInfo:
  def empty: ExtraClassInfo = ExtraClassInfo(None, Map.empty)
