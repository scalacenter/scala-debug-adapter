package ch.epfl.scala.debugadapter.internal.jdi

class JdiLocation(val obj: Any) extends JavaReflection(obj, "com.sun.jdi.Location"):
  def lineNumber: Int = invokeMethod[Int]("lineNumber")
