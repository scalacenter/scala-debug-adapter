package ch.epfl.scala.debugadapter.internal.scalasig

import scala.reflect.internal.pickling.PickleFormat._

/**
 * Originally copied from https://github.com/JetBrains/intellij-scala
 * https://github.com/JetBrains/intellij-scala/blob/074e8f98d9789b3e7def3ade8d39e7ae770beccf/scala/decompiler/src/org/jetbrains/plugins/scala/decompiler/scalasig/TagGroups.scala
 *
 * Nikolay.Tropin
 * 19-Jul-17
 */
object TagGroups {
  def isSymbolTag(tag: Int): Boolean =
    firstSymTag <= tag && tag <= lastExtSymTag

  def isConstantTag(tag: Int): Boolean =
    tag >= LITERALunit && tag <= LITERALenum

  def isAnnotArgTag(tag: Int): Boolean = tag == TREE || isConstantTag(tag)

  def isConstAnnotArgTag(tag: Int): Boolean = isConstantTag(
    tag
  ) || tag == TREE || tag == ANNOTINFO || tag == ANNOTATEDtree

  def isTypeTag(tag: Int): Boolean =
    tag >= NOtpe && tag <= IMPLICITMETHODtpe ||
      tag == ANNOTATEDtpe || tag == DEBRUIJNINDEXtpe ||
      tag == EXISTENTIALtpe || tag == SUPERtpe

  def isNameTag(tag: Int): Boolean = tag == TERMname || tag == TYPEname

  final val SUPERtpe2 = 52 // There is an inconsistency in PicklerFormat
}
