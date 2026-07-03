package ch.epfl.scala.debugadapter.sbtplugin.internal

import sjsonnew.JsonFormat
import JsonProtocol.given

// Re-exposes the contraband-generated JSON formats as plain implicits so that the
// shared plugin sources can import them uniformly across sbt 1 (implicit) and sbt 2 (given).
// The generated instances are referenced by name: resolving them via `summon` would pick
// the locally-defined `implicit val` (which outranks the `given` import), yielding a self
// reference whose delegate reader/writer is null at runtime.
private[sbtplugin] object JsonProtocolCompat {
  implicit val scalaMainClassFormat: JsonFormat[ScalaMainClass] = JsonProtocol.ScalaMainClassFormat
  implicit val scalaTestSuitesFormat: JsonFormat[ScalaTestSuites] = JsonProtocol.ScalaTestSuitesFormat
  implicit val debugSessionParamsFormat: JsonFormat[DebugSessionParams] = JsonProtocol.DebugSessionParamsFormat
  implicit val debugSessionAddressFormat: JsonFormat[DebugSessionAddress] = JsonProtocol.DebugSessionAddressFormat
  implicit val arrayStringFormat: JsonFormat[Array[String]] = JsonProtocol.arrayFormat[String]
}
