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
import ch.epfl.scala.debugadapter.ScalaVersion
import scala.jdk.OptionConverters._

class Scala3UnpicklerBridge(
    scalaVersion: ScalaVersion,
    bridge: Any,
    skipMethod: Method,
    formatMethod: Method,
    testMode: Boolean
) extends ScalaUnpickler(scalaVersion, testMode) {
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
  def tryLoad(
      debuggee: Debuggee,
      classLoader: ClassLoader,
      logger: Logger,
      testMode: Boolean
  ): Try[Scala3UnpicklerBridge] = {
    Try {
      val className = "ch.epfl.scala.debugadapter.internal.stacktrace.Scala3Unpickler"
      val cls = classLoader.loadClass(className)
      val ctr = cls.getConstructor(classOf[Array[Path]], classOf[Consumer[String]], classOf[Boolean])

      // TASTy Query needs the javaRuntimeJars
      val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
        case Java8(_, classJars, _) => classJars
        case java9OrAbove: Java9OrAbove =>
          java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))
      }
      val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
      val warnLogger: Consumer[String] = msg => logger.warn(msg)
      val bridge = ctr.newInstance(
        debuggeeClasspath,
        warnLogger,
        testMode: java.lang.Boolean
      )
      val skipMethod = cls.getMethods.find(m => m.getName == "skipMethod").get
      val formatMethod = cls.getMethods.find(m => m.getName == "formatMethod").get

      new Scala3UnpicklerBridge(debuggee.scalaVersion, bridge, skipMethod, formatMethod, testMode)
    }
  }
}
