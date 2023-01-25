package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.*
import scala.util.Try

private[internal] class MessageLogger() {
  def log(logMessage: PlainLogMessage, frame: FrameReference): Try[Value] = {
    val result = for {
      classLoader <- JdiClassLoader.fromFrame(frame)
      vm = frame.thread.virtualMachine()
      arg <- Safe(vm.mirrorOf(logMessage.message))
      predefClass <- classLoader.loadClass("scala.Predef$")
      moduleField <- predefClass
        .invoke("getDeclaredField", List(vm.mirrorOf("MODULE$")))
        .map(JdiObject(_, frame.thread))
      predef <- moduleField.invoke("get", List(null)).map(JdiObject(_, frame.thread))
      res <- predef.invoke("println", "(Ljava/lang/Object;)V", List(arg))
    } yield res
    result.getResult
  }
}
