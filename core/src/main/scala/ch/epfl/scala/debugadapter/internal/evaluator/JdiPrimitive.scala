package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.{ObjectReference, ThreadReference}

object JdiPrimitive {
  def boxed(value: AnyVal, classLoader: JdiClassLoader, thread: ThreadReference): Option[JdiObject] = {
    val vm = thread.virtualMachine()
    val jdiValue = vm.mirrorOf(value.toString)
    for {
      clazz <- value match {
        case _: Boolean => classLoader.loadClass("java.lang.Boolean")
        case _: Byte => classLoader.loadClass("java.lang.Byte")
        case _: Char => classLoader.loadClass("java.lang.Character")
        case _: Double => classLoader.loadClass("java.lang.Double")
        case _: Float => classLoader.loadClass("java.lang.Float")
        case _: Int => classLoader.loadClass("java.lang.Integer")
        case _: Long => classLoader.loadClass("java.lang.Long")
        case _: Short => classLoader.loadClass("java.lang.Short")
      }
      jdiObject <- clazz
        .invokeStatic("valueOf", List(jdiValue))
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
    } yield jdiObject
  }
}
