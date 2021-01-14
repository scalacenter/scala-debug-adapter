package sbt.internal.bsp.codec

trait DebugAdapterJsonProtocol extends JsonProtocol
  with sbt.internal.bsp.codec.DebugSessionAddressFormats
  with sbt.internal.bsp.codec.DebugSessionParamsFormats

object DebugAdapterJsonProtocol extends DebugAdapterJsonProtocol