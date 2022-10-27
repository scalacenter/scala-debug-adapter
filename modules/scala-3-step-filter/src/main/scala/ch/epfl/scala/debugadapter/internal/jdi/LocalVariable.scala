package ch.epfl.scala.debugadapter.internal.jdi

class LocalVariable(obj: Any) extends JavaReflection(obj, "com.sun.jdi.LocalVariable"):
  def name: String = invokeMethod("name")
  def `type`: Type =
    Type(invokeMethod("type"))
