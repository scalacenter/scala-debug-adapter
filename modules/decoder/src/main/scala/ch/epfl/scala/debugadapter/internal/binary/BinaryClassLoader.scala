package ch.epfl.scala.debugadapter.internal.binary

trait BinaryClassLoader:
  def loadClass(name: String): ClassType
