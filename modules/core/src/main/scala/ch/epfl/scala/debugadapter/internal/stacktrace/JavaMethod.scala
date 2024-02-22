package ch.epfl.scala.debugadapter.internal.stacktrace

import com.sun.jdi
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import scala.jdk.CollectionConverters.*

final case class JavaMethod(method: jdi.Method, isGenerated: Boolean) extends DecodedMethod {
  def format: String = {
    val declaringType = method.declaringType.name.split("\\.").last
    val argumentTypes = method.argumentTypeNames.asScala
      .map(arg => arg.split("\\.").last)
      .mkString(",")
    val returnType = method.returnTypeName.split("\\.").last
    s"$declaringType.${method.name}($argumentTypes): $returnType"
  }
}
