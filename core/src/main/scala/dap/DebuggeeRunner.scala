package dap

import java.nio.file.Path
import scala.concurrent.Future
import scala.util.Try
import java.net.InetSocketAddress

trait DebuggeeRunner {
  def logger: Logger
  def run(callbacks: DebugSessionCallbacks): Cancelable
  def classFilesMappedTo(origin: Path, lines: Array[Int], columns: Array[Int]): List[Path]
}
