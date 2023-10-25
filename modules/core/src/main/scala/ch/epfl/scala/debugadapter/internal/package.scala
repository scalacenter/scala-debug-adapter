package ch.epfl.scala.debugadapter

package object internal {
  val runtimeFilterName = "runtime"
  val decoderFilterName = "decoder"
  val classLoadingFilterName = "classLoader"
  val defaultFiltersName = Array(runtimeFilterName, classLoadingFilterName, decoderFilterName)
  val defaultFilters = defaultFiltersName.map(_ -> true).toMap
}
