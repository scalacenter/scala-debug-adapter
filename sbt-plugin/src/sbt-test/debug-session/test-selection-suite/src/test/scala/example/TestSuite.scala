package example

import org.junit.Test

class TestSuite {

  @Test
  def test1(): Unit = {
    val mem = Runtime.getRuntime().maxMemory()
    val env = System.getenv("KEY")
    val out = s"test1_${mem}_$env"
    println(out)
  }

  @Test
  def test2(): Unit = {
    println("test2")
  }
}
