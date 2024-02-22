package ch.epfl.scala.debugadapter.internal.stacktrace

class DecodedMethodBridge(method: DecodedMethod, formatter: StackTraceFormatter):
  def format: String = formatter.format(method)
  def isGenerated: Boolean = method.isGenerated
