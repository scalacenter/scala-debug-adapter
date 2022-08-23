package ch.epfl.scala.debugadapter.internal.stepfilter

import java.lang.reflect.Method
import ch.epfl.scala.debugadapter.Logger
import com.sun.jdi
import scala.util.Try

class Scala3StepFilter(
    bridge: Any,
    skipMethod: Method,
    logger: Logger,
    testMode: Boolean
) extends ScalaVersionStepFilter {
  override def skipMethod(method: jdi.Method): Boolean =
    skipMethod.invoke(bridge, method).asInstanceOf[Boolean]
}

object Scala3StepFilter {
  def tryLoad(
      classLoader: ClassLoader,
      logger: Logger,
      testMode: Boolean
  ): Try[Scala3StepFilter] = {
    Try {
      val cls = classLoader.loadClass("ch.epfl.debugadapter.StepFilterBridge")
      val ctr = cls.getConstructor()
      val bridge = ctr.newInstance()
      val skipMethod = cls.getMethods.find(m => m.getName == "skipMethod").get
      new Scala3StepFilter(bridge, skipMethod, logger, testMode)
    }
  }
}
