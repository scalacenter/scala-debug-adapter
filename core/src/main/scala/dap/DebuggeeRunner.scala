package dap

import java.nio.file.Path

trait DebuggeeRunner {
  def logger: Logger
  def run(callbacks: DebugSessionCallbacks): Cancelable
  def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path]
}
