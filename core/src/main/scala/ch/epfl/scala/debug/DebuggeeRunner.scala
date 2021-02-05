package ch.epfl.scala.debug

import java.nio.file.Path

trait DebuggeeRunner {
  def name: String
  def run(listener: DebuggeeListener): CancelableFuture[Unit]
  def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path]
}
