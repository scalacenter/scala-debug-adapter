package ch.epfl.scala.debugadapter.internal.jdi

class ReferenceType(obj: Any) extends Type(obj, "com.sun.jdi.ReferenceType"):
  def isObject = isPackageObject || name.endsWith("$")
  def isPackageObject = name.endsWith(".package") || name.endsWith("$package")
