package ch.epfl.scala.debugadapter.internal.binary

trait Field extends Symbol:
  def declaringClass: ClassType
  def `type`: Type
