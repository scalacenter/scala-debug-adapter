package ch.epfl.scala.debugadapter.internal.stacktrace

import java.lang.reflect.Method
import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi
import ch.epfl.scala.debugadapter.Debuggee
import java.util.function.Consumer
import java.lang.reflect.InvocationTargetException
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import scala.util.Try
import java.nio.file.Path
import java.util.Optional
import scala.jdk.OptionConverters._

class Scala3UnpicklerBridge(
    debuggee: Debuggee,
    cls: Class[?],
    var bridge: Any,
    skipMethod: Method,
    formatMethod: Method,
    testMode: Boolean,
    logger: Logger
) extends ScalaUnpickler(debuggee.scalaVersion, testMode) {

  override def reload(): Unit =
    bridge = Scala3UnpicklerBridge.load(debuggee, cls, logger, testMode)

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

object Scala3UnpicklerBridge {
  def load(debuggee: Debuggee, unpicklerClass: Class[?], logger: Logger, testMode: Boolean) = {
    // TASTy Query needs the javaRuntimeJars
    val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))
    }

    val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
    val warnLogger: Consumer[String] = msg => logger.warn(msg)
    val ctr = unpicklerClass.getConstructor(classOf[Array[Path]], classOf[Consumer[String]], classOf[Boolean])

    ctr.newInstance(debuggeeClasspath, warnLogger, testMode: java.lang.Boolean)
  }

  def tryLoad(
      debuggee: Debuggee,
      classLoader: ClassLoader,
      logger: Logger,
      testMode: Boolean
  ): Try[Scala3UnpicklerBridge] = {
    Try {
      val className = "ch.epfl.scala.debugadapter.internal.stacktrace.Scala3Unpickler"
      val cls = classLoader.loadClass(className)

      val bridge = load(debuggee, cls, logger, testMode)
      val skipMethod = cls.getMethod("skipMethod", classOf[Any])
      val formatMethod = cls.getMethod("formatMethod", classOf[Any])

      new Scala3UnpicklerBridge(debuggee, cls, bridge, skipMethod, formatMethod, testMode, logger)
    }
  }
}
