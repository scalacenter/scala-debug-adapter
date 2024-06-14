package ch.epfl.scala.debugadapter.internal.jdi
import ch.epfl.scala.decoder.binary.*

class JdiType(obj: Any, className: String = "com.sun.jdi.Type") extends JavaReflection(obj, className) with Type:
  override def name: String = invokeMethod("name")
  override def sourceLines: Option[SourceLines] = None
