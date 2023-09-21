package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.debugadapter.internal.binary.*

class JdiLocalVariable(obj: Any) extends JavaReflection(obj, "com.sun.jdi.LocalVariable") with Parameter:
  override def name: String = invokeMethod("name")
  override def sourceLines: Seq[Int] = Seq.empty
  override def `type`: Type = JdiType(invokeMethod("type"))
