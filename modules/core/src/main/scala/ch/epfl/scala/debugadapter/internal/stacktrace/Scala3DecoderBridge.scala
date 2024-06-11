package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.Errors
import com.microsoft.java.debug.core.adapter.stacktrace.DecodedMethod
import com.sun.jdi

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.nio.file.Files
import java.nio.file.Path
import java.util.function.Consumer
import scala.jdk.CollectionConverters.*

private class Scala3DecoderBridge(
    instance: Any,
    decodeMethod: Method
) {
  def decode(method: jdi.Method): DecodedMethod =
    try new DecodedMethodBridge(decodeMethod.invoke(instance, method))
    catch {
      case e: InvocationTargetException => throw Errors.frameDecodingFailure(e.getCause)
    }
}

private object Scala3DecoderBridge {
  def apply(debuggee: Debuggee, classLoader: ClassLoader, logger: Logger, testMode: Boolean): Scala3DecoderBridge = {
    val className = "ch.epfl.scala.debugadapter.internal.Scala3DecoderBridge"
    val cls = classLoader.loadClass(className)
    val instance = newInstance(debuggee, cls, logger, testMode)
    val decodeMethod = cls.getMethod("decode", classOf[jdi.Method])
    new Scala3DecoderBridge(instance, decodeMethod)
  }

  private def newInstance(debuggee: Debuggee, decoderClass: Class[?], logger: Logger, testMode: Boolean) = {
    val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.flatMap { javaFs =>
          Files.list(javaFs.fileSystem.getPath("/modules")).iterator.asScala.toSeq
        }
    }
    val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
    val warnLogger: Consumer[String] = msg => logger.warn(msg)
    val ctr = decoderClass.getConstructor(classOf[Array[Path]], classOf[Consumer[String]], classOf[Boolean])

    ctr.newInstance(debuggeeClasspath, warnLogger, testMode: java.lang.Boolean)
  }
}
