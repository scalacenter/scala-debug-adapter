package ch.epfl.scala.debugadapter.testfmk

import java.lang.management.ManagementFactory
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Properties
import ch.epfl.scala.debugadapter.JavaRuntime

trait CommonFunSuite extends munit.FunSuite {
  def javaRuntime = JavaRuntime(Properties.jdkHome).get

  def isJava8: Boolean =
    Properties.javaVersion.startsWith("1.8")

  def isDebug: Boolean = {
    val mxBean = ManagementFactory.getRuntimeMXBean
    mxBean.getInputArguments.asScala.exists(_.contains("jdwp"))
  }

  override def munitTimeout: Duration =
    if (isDebug) 8.hours else super.munitTimeout
}
