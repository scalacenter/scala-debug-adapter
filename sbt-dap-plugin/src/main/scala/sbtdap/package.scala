package object sbtdap {
  private[sbtdap] type ProtocolError = JsonRpcResponse.Error
  private[sbtdap] type BspResponse[A] = Either[ProtocolError, A]
}
