package ch.epfl.scala.debugadapter.internal.javareflect

import ch.epfl.scala.debugadapter.internal.binary.SourceLines
import ch.epfl.scala.debugadapter.internal.binary.SignedName

private final case class ExtraClassInfo(
    sourceLines: Option[SourceLines],
    methodsInfo: Map[SignedName, ExtraMethodInfo]
):
  def getMethodInfo(sig: SignedName): ExtraMethodInfo = methodsInfo.getOrElse(sig, ExtraMethodInfo.empty)

private object ExtraClassInfo:
  def empty: ExtraClassInfo = ExtraClassInfo(None, Map.empty)
