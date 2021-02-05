package dap

import java.nio.file.Path

trait DebuggeeRunner {
  def name: String
  def run(logger: DebuggeeLogger): CancelableFuture[Unit]
  def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path]
}
