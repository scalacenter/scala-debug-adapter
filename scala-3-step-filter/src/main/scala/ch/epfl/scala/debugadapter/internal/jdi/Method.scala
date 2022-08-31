package ch.epfl.scala.debugadapter.internal.jdi

class Method(val obj: Any) extends JavaReflection:
  def name: String = invokeMethod("name")

  def declaringType: ReferenceType =
    ReferenceType(invokeMethod("declaringType"))

  override def toString: String = invokeMethod("toString")
