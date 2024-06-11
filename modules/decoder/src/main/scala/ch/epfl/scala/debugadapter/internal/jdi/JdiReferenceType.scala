package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.decoder.binary.*

import java.util as ju
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

class JdiReferenceType(obj: Any, className: String = "com.sun.jdi.ReferenceType")
    extends JdiType(obj, className)
    with ClassType:
  override def classLoader: BinaryClassLoader = JdiClassLoader(invokeMethod("classLoader"))
  override def superclass: Option[ClassType] = if isClass then asClass.superclass else asInterface.superclass
  override def interfaces: Seq[ClassType] = if isClass then asClass.interfaces else asInterface.interfaces
  def isClass = isInstanceOf("com.sun.jdi.ClassType")
  override def isInterface = isInstanceOf("com.sun.jdi.InterfaceType")
  override def sourceLines: Option[SourceLines] =
    Some(SourceLines(sourceName, allLineLocations.map(_.lineNumber)))

  override def method(name: String, sig: String): Option[Method] =
    visibleMethods.find(_.signedName == SignedName(name, sig))

  override def declaredMethod(name: String, sig: String): Option[Method] =
    declaredMethods.find(_.signedName == SignedName(name, sig))

  override def declaredMethods: Seq[Method] =
    invokeMethod[ju.List[Any]]("methods").asScala.map(JdiMethod(_)).toSeq

  override def declaredField(name: String): Option[Field] = None

  def asClass: JdiClassType = JdiClassType(obj)
  def asInterface: JdiInterfaceType = JdiInterfaceType(obj)
  def constantPool: ConstantPool = ConstantPool(invokeMethod("constantPool"))

  private def allLineLocations: Seq[JdiLocation] =
    try invokeMethod[ju.List[Any]]("allLineLocations").asScala.map(JdiLocation(_)).toSeq
    catch
      case e: Exception if e.getClass.getName == "com.sun.jdi.AbsentInformationException" =>
        Seq.empty

  private[jdi] def sourceName: String = invokeMethod("sourceName")

  private def visibleMethods: Seq[JdiMethod] =
    invokeMethod[ju.List[Any]]("visibleMethods").asScala.map(JdiMethod(_)).toSeq

class JdiClassType(obj: Any) extends JdiReferenceType(obj, "com.sun.jdi.ClassType"):
  override def superclass: Option[ClassType] = Some(JdiReferenceType(invokeMethod[Any]("superclass")))
  override def interfaces: Seq[ClassType] =
    invokeMethod[java.util.List[Any]]("interfaces").asScala.toSeq.map(JdiReferenceType(_))

class JdiInterfaceType(obj: Any) extends JdiReferenceType(obj, "com.sun.jdi.InterfaceType"):
  override def interfaces: Seq[ClassType] =
    invokeMethod[java.util.List[Any]]("superinterfaces").asScala.toSeq.map(JdiReferenceType(_))

  override def superclass: Option[ClassType] = None
