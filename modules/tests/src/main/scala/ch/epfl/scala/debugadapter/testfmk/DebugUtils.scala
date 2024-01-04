package ch.epfl.scala.debugadapter.testfmk

import java.lang.management.ManagementFactory
import scala.jdk.CollectionConverters.*

object DebugUtils {
  def isDebug: Boolean = {
    val mxBean = ManagementFactory.getRuntimeMXBean
    mxBean.getInputArguments.asScala.exists(_.contains("jdwp"))
  }
}
