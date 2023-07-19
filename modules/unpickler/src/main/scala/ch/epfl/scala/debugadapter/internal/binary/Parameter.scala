package ch.epfl.scala.debugadapter.internal.binary

trait Parameter:
  def name: String
  def `type`: Type
