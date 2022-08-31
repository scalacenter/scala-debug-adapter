package ch.epfl.scala.debugadapter.internal.jdi

class ReferenceType(val obj: Any) extends JavaReflection:
  def name: String = invokeMethod("name")
