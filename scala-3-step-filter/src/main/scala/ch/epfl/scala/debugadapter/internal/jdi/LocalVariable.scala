package ch.epfl.scala.debugadapter.internal.jdi

class LocalVariable(val obj: Any) extends JavaReflection:
  def name: String = invokeMethod("name")
  def `type`: Type =
    Type(invokeMethod("type"))
