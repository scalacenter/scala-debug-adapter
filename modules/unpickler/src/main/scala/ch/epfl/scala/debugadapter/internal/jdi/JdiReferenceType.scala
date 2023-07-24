package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.debugadapter.internal.binary.*
import scala.jdk.CollectionConverters.*

class JdiReferenceType(obj: Any, className: String = "com.sun.jdi.ReferenceType")
    extends JdiType(obj, className)
    with ClassType:
  override def superclass: ClassType = asClass.superclass
  override def interfaces: Seq[ClassType] = asClass.interfaces
  def asClass: JdiClassType = JdiClassType(obj)

class JdiClassType(obj: Any) extends JdiReferenceType(obj, "com.sun.jdi.ClassType"):
  override def superclass: ClassType = JdiReferenceType(invokeMethod[Any]("superclass"))
  override def interfaces: Seq[ClassType] =
    invokeMethod[java.util.List[Any]]("interfaces").asScala.toSeq.map(JdiReferenceType(_))
