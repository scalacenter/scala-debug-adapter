package example

import java.nio.file.Files
import scala.util.control.NonFatal

object NativeCall extends App {
  val env = org.lmdbjava.Env.create()
  val folder = Files.createTempDirectory("test").toFile
  env.open(folder)
  println("Success")
}
