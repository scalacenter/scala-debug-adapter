package ch.epfl.scala.debugadapter.internal.binary

trait Parameter extends Symbol:
  def `type`: Type

  def isThis: Boolean = name == "$this"
  def isOuter: Boolean = name == "$outer"
  def isCapture: Boolean = !name.matches("_\\$\\d+") && name.matches(".+\\$\\d+")
  def isUnknownJavaArg: Boolean = name.matches("arg\\d+")
  def isJavaLangEnumParam: Boolean = name == "_$name" || name == "_$ordinal"
  def isGenerated: Boolean = isCapture || isOuter || isThis || isUnknownJavaArg || isJavaLangEnumParam
