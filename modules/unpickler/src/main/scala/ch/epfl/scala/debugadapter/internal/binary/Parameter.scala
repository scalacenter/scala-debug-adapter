package ch.epfl.scala.debugadapter.internal.binary

trait Parameter:
  def name: String
  def `type`: Type

  def isThis: Boolean = name == "$this"
  def isOuter: Boolean = name == "$outer"
  def isCapture: Boolean = name.matches(".*\\$\\d+")
