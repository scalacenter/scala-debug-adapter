package ch.epfl.scala.debugadapter.internal.stepfilter

import java.lang.reflect.Method
import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi
import ch.epfl.scala.debugadapter.DebuggeeRunner
import java.util.function.Consumer
import java.nio.file.Path
import java.lang.reflect.InvocationTargetException
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import scala.util.control.NonFatal
import scala.util.Try
import scala.util.Success
import ch.epfl.scala.debugadapter.testing.SingleTestResult
import scala.util.Failure

class Scala3StepFilter(
    bridge: Any,
    skipMethod: Method,
    logger: Logger,
    testMode: Boolean
) extends ScalaStepFilter {
  override protected def skipScalaMethod(method: jdi.Method): Boolean =
    try {
      skipMethod.invoke(bridge, method).asInstanceOf[Boolean]
    } catch {
      case e: InvocationTargetException => throw e.getCause
    }

}

object Scala3StepFilter {
  def tryLoad(
      runner: DebuggeeRunner,
      logger: Logger,
      testMode: Boolean
  ): Option[Scala3StepFilter] = {
    for {
      classLoader <- runner.stepFilterClassLoader
      stepFilter <- Try {
        val className =
          "ch.epfl.scala.debugadapter.internal.stepfilter.ScalaStepFilterBridge"
        val cls = classLoader.loadClass(className)
        val ctr = cls.getConstructor(
          classOf[Array[Path]],
          classOf[Consumer[String]],
          classOf[Boolean]
        )
        // TASTy Query needs the javaRuntimeJars
        val javaRuntimeJars = runner.javaRuntime.toSeq.flatMap {
          case Java8(_, classJars, _) => classJars
          case java9OrAbove: Java9OrAbove =>
            java9OrAbove.classSystems
              .map(_.fileSystem.getPath("/modules", "java.base"))
        }
        val debuggeeClasspath = runner.classPath.toArray ++ javaRuntimeJars
        val warnLogger: Consumer[String] = msg => logger.warn(msg)
        val bridge = ctr.newInstance(
          debuggeeClasspath,
          warnLogger,
          testMode: java.lang.Boolean
        )
        val skipMethod = cls.getMethods.find(m => m.getName == "skipMethod").get
        new Scala3StepFilter(bridge, skipMethod, logger, testMode)
      } match {
        case Success(value) => Some(value)
        case Failure(e) =>
          logger.error("Failed to load step filter from provided class loader")
          logger.trace(e)
          None
      }
    } yield stepFilter
  }
}
