package ch.epfl.scala.debugadapter.internal.jdi

import scala.jdk.CollectionConverters.*
import java.lang.reflect.InvocationTargetException

class Method(val obj: Any) extends JavaReflection(obj, "com.sun.jdi.Method"):
  def name: String = invokeMethod("name")

  def declaringType: ReferenceType =
    ReferenceType(invokeMethod("declaringType"))

  def arguments: Seq[LocalVariable] =
    invokeMethod[java.util.List[Object]]("arguments").asScala.toSeq
      .map(LocalVariable.apply(_))

  def returnType: Option[Type] =
    try Some(Type(invokeMethod("returnType")))
    catch
      case e: InvocationTargetException if e.getCause.getClass.getName == "com.sun.jdi.ClassNotLoadedException" =>
        None

  def isExtensionMethod: Boolean =
    name.endsWith("$extension")

  def isTraitInitializer: Boolean =
    name == "$init$"

  def isClassInitializer: Boolean =
    name == "<init>"

  override def toString: String = invokeMethod("toString")
