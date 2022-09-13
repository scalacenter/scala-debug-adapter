package ch.epfl.scala.debugadapter.internal.jdi

class Type(obj: Any, className: String = "com.sun.jdi.Type")
    extends JavaReflection(obj, className):
  def name: String = invokeMethod("name")
