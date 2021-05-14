package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.{ObjectReference, ThreadReference}

object JdiPrimitive {
  def boxed(value: Int, classLoader: JdiClassLoader, thread: ThreadReference): Option[JdiObject] = {
    val vm = thread.virtualMachine()
    val jdiValue = vm.mirrorOf(value.toString)
    for {
      integerClass <- classLoader.loadClass("java.lang.Integer")
      jdiObject <- integerClass
        .invokeStatic("valueOf", List(jdiValue))
        .map(_.asInstanceOf[ObjectReference])
        .map(new JdiObject(_, thread))
    } yield jdiObject
  }
}
