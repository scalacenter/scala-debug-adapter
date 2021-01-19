package dap

import java.nio.file.Path

trait DebuggeeRunner {
  def logger: Logger
  def run(callbacks: DebugSessionCallbacks): CancelableFuture[Unit]
  def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path]
}
