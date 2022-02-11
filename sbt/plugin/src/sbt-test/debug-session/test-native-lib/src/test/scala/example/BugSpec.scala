package example

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File

class BugSpec extends AnyFlatSpec {
  it should "have classpath" in {
    val classPath = System.getProperties().get("java.class.path")
    val lines = classPath.toString().split(":").mkString("\n")
    println(lines)

    val env = org.lmdbjava.Env.create()
    val folder = new File("/tmp/test")
    folder.mkdirs()
    println(env.open(folder))
  }
}
