package ch.epfl.scala.debugadapter.internal.binary

trait Parameter extends Symbol:
  def `type`: Type

  def isThis: Boolean = name == "$this"
  def isOuter: Boolean = name == "$outer"
  def isCapture: Boolean = name.matches(".+\\$\\d+") || name.matches("arg\\d+")
  def isGenerated: Boolean = isCapture || isOuter || isThis
