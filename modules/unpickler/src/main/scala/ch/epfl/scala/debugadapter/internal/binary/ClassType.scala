package ch.epfl.scala.debugadapter.internal.binary

trait ClassType extends Type:
  def name: String

  def isObject = isPackageObject || name.endsWith("$")
  def isPackageObject = name.endsWith(".package") || name.endsWith("$package")
