package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi

import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.nio.file.Files
import java.nio.file.Path
import java.util.Optional
import java.util.function.Consumer
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.Try

class Scala3DecoderBridge(
    debuggee: Debuggee,
    cls: Class[?],
    var bridge: Any,
    skipMethod: Method,
    formatMethod: Method,
    testMode: Boolean,
    logger: Logger
) extends ScalaDecoder(debuggee.scalaVersion, testMode) {

  override def reload(): Unit =
    bridge = Scala3DecoderBridge.load(debuggee, cls, logger, testMode)

  override protected def skipScala(method: jdi.Method): Boolean = {
    try skipMethod.invoke(bridge, method).asInstanceOf[Boolean]
    catch {
      case e: InvocationTargetException => throw e.getCause
    }
  }

  override def formatScala(method: jdi.Method): Option[String] = {
    try
      formatMethod.invoke(bridge, method).asInstanceOf[Optional[String]].toScala

    catch {
      case e: InvocationTargetException => throw e.getCause
    }
  }
}

object Scala3DecoderBridge {
  def load(debuggee: Debuggee, decoderClass: Class[?], logger: Logger, testMode: Boolean) = {
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

  def tryLoad(
      debuggee: Debuggee,
      classLoader: ClassLoader,
      logger: Logger,
      testMode: Boolean
  ): Try[Scala3DecoderBridge] = {
    Try {
      val className = "ch.epfl.scala.debugadapter.internal.stacktrace.Scala3DecoderBridge"
      val cls = classLoader.loadClass(className)

      val bridge = load(debuggee, cls, logger, testMode)
      val skipMethod = cls.getMethod("skipMethod", classOf[Any])
      val formatMethod = cls.getMethod("formatMethod", classOf[Any])

      new Scala3DecoderBridge(debuggee, cls, bridge, skipMethod, formatMethod, testMode, logger)
    }
  }
}
