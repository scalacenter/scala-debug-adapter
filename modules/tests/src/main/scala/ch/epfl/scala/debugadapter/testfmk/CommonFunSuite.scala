package ch.epfl.scala.debugadapter.testfmk

import scala.concurrent.duration.*
import scala.util.Properties

trait CommonFunSuite extends munit.FunSuite with CommonUtils {
  override def munitTimeout: Duration =
    defaultTimeout(super.munitTimeout)
}

trait CommonUtils {
  def isJava8: Boolean = Properties.javaVersion.startsWith("1.8")

  def isDebug: Boolean = DebugUtils.isDebug

  def defaultTimeout(timeout: Duration) = if (isDebug) 8.hours else timeout
}
