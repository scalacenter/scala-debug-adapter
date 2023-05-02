package ch.epfl.scala.debugadapter.internal.evaluator

import com.sun.jdi.*

import scala.util.Try

private[internal] class MessageLogger() {
  def log(logMessage: PlainLogMessage, frame: JdiFrame): Try[Value] = {
    val result = for {
      classLoader <- frame.classLoader()
      arg <- classLoader.mirrorOf(logMessage.message)
      predefClass <- classLoader.loadClass("scala.Predef$")
      predef <- predefClass.getStaticField("MODULE$").map(_.asObject)
      res <- predef.invoke("println", "(Ljava/lang/Object;)V", List(arg))
    } yield res.value
    result.getResult
  }
}
