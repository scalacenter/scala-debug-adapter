package ch.epfl.scala.debugadapter.internal;

import com.google.gson.JsonObject
import scala.jdk.CollectionConverters.*

class PartialLaunchArguments {
  val noDebug = false
}

object PartialLaunchArguments {
  def extractStepFilers(obj: JsonObject): Map[String, Boolean] =
    try
      obj
        .getAsJsonObject("scalaStepFilters")
        .entrySet()
        .asScala
        .map { entry =>
          entry.getKey -> entry.getValue.getAsBoolean
        }
        .toMap
    catch {
      case _: Throwable =>
        Map(
          decoderFilterName -> true,
          classLoadingFilterName -> true,
          runtimeFilterName -> true
        )
    }
}
