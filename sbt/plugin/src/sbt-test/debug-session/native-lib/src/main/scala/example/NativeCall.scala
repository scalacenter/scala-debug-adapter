package example

import java.nio.file.Files
import scala.util.control.NonFatal
import scala.util.Properties

object NativeCall extends App {
  if (Properties.javaVersion.startsWith("17")) {
    // There is no support for lmdbjava in Java 17
    ()
  } else {
    val env = org.lmdbjava.Env.create()
    val folder = Files.createTempDirectory("test").toFile
    env.open(folder)
  }
  println("Success")
}
