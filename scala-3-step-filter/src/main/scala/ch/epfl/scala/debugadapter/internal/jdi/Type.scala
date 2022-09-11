package ch.epfl.scala.debugadapter.internal.jdi

class Type(val obj: Any) extends JavaReflection:
  def name: String = invokeMethod("name")
