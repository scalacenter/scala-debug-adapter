package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.debugadapter.internal.binary.*
import scala.jdk.CollectionConverters.*
import java.util as ju

class JdiReferenceType(obj: Any, className: String = "com.sun.jdi.ReferenceType")
    extends JdiType(obj, className)
    with ClassType:
  override def superclass: Option[ClassType] = if isClass then asClass.superclass else asInterface.superclass
  override def interfaces: Seq[ClassType] = if isClass then asClass.interfaces else asInterface.interfaces
  def isClass = isInstanceOf("com.sun.jdi.ClassType")
  override def isInterface = isInstanceOf("com.sun.jdi.InterfaceType")
  override def sourceLines: Seq[SourceLine] =
    allLineLocations.map(_.lineNumber).distinct.sorted.map(SourceLine(_))

  override def declaredMethod(name: String, sig: String): Option[Method] = None

  def asClass: JdiClassType = JdiClassType(obj)
  def asInterface: JdiInterfaceType = JdiInterfaceType(obj)

  private def allLineLocations: Seq[JdiLocation] =
    invokeMethod[ju.List[Any]]("allLineLocations").asScala.map(JdiLocation.apply(_)).toSeq

class JdiClassType(obj: Any) extends JdiReferenceType(obj, "com.sun.jdi.ClassType"):
  override def superclass: Option[ClassType] = Some(JdiReferenceType(invokeMethod[Any]("superclass")))
  override def interfaces: Seq[ClassType] =
    invokeMethod[java.util.List[Any]]("interfaces").asScala.toSeq.map(JdiReferenceType(_))

class JdiInterfaceType(obj: Any) extends JdiReferenceType(obj, "com.sun.jdi.InterfaceType"):
  override def interfaces: Seq[ClassType] =
    invokeMethod[java.util.List[Any]]("superinterfaces").asScala.toSeq.map(JdiReferenceType(_))

  override def superclass: Option[ClassType] = None
