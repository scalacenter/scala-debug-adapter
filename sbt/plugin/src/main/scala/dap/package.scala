package object dap {
  private[dap] type Result[A] = Either[Error, A]
}
