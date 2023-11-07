package ch.epfl.scala.debugadapter.testfmk

import java.lang.management.ManagementFactory
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

trait DebuggableFunSuite extends munit.FunSuite {
  private def isDebug: Boolean = {
    val mxBean = ManagementFactory.getRuntimeMXBean
    mxBean.getInputArguments.asScala.exists(_.contains("jdwp"))
  }

  override def munitTimeout: Duration =
    if (isDebug) 8.hours else super.munitTimeout
}
