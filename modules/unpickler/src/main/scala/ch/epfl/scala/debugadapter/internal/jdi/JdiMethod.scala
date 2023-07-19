package ch.epfl.scala.debugadapter.internal.jdi

import ch.epfl.scala.debugadapter.internal.binary.*

import java.lang.reflect.InvocationTargetException
import scala.jdk.CollectionConverters.*

class JdiMethod(val obj: Any) extends JavaReflection(obj, "com.sun.jdi.Method") with Method:
  override def name: String = invokeMethod("name")

  override def declaringClass: ClassType =
    JdiReferenceType(invokeMethod("declaringType"))

  override def parameters: Seq[Parameter] =
    invokeMethod[java.util.List[Object]]("arguments").asScala.toSeq.map(JdiLocalVariable.apply(_))

  override def returnType: Option[Type] =
    try Some(JdiType(invokeMethod("returnType")))
    catch
      case e: InvocationTargetException if e.getCause.getClass.getName == "com.sun.jdi.ClassNotLoadedException" =>
        None

  override def returnTypeName: String = invokeMethod("returnTypeName")

  override def toString: String = invokeMethod("toString")
