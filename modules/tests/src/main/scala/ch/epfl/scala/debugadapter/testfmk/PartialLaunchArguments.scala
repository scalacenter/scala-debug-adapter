package ch.epfl.scala.debugadapter.testfmk

import com.microsoft.java.debug.core.protocol.Requests.LaunchArguments
import java.{util => ju}

class PartialLaunchArguments(val scalaStepFilters: ju.Map[String, Boolean]) extends LaunchArguments
