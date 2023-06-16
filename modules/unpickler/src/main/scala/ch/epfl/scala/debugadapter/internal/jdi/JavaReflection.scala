package ch.epfl.scala.debugadapter.internal.jdi

import scala.reflect.ClassTag

private[jdi] class JavaReflection(obj: Any, className: String):
  private def cls = obj.getClass.getClassLoader.loadClass(className)

  protected def invokeMethod[T](name: String): T =
    val method = cls.getMethod(name)
    method.invoke(obj).asInstanceOf[T]
