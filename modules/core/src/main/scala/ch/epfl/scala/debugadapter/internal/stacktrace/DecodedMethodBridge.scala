package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.Errors
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod

import java.lang.reflect.InvocationTargetException

class DecodedMethodBridge(instance: Any) extends DecodedMethod {

  override def format(): String = invoke[String]("format")

  override def isGenerated(): Boolean = invoke[Boolean]("isGenerated")

  private def invoke[T](methodName: String): T =
    try instance.getClass.getMethod(methodName).invoke(instance).asInstanceOf[T]
    catch {
      case e: InvocationTargetException =>
        throw Errors.frameDecodingFailure(e.getCause)
    }
}
