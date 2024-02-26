package ch.epfl.scala.debugadapter.internal.jdi

import scala.reflect.ClassTag
import java.lang.reflect.InvocationTargetException

private[jdi] trait JavaReflection(obj: Any, className: String):
  private val classLoader = obj.getClass.getClassLoader

  // Impl classes are private
  private def cls = classLoader.loadClass(className)

  protected def invokeMethod[T](name: String): T =
    try
      val method = cls.getMethod(name)
      method.invoke(obj).asInstanceOf[T]
    catch case e: InvocationTargetException => throw e.getCause

  protected def isInstanceOf(className: String): Boolean = classLoader.loadClass(className).isInstance(obj)

  override def toString: String = obj.toString
