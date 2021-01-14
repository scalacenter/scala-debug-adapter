package dap

trait ExitStatus {
  def isOk: Boolean
  def name: String
}
