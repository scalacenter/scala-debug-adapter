package scaladebug.test

object BreakpointTest {
  def main(args: Array[String]): Unit = {
    println("Breakpoint in main method")
    val h = new Hello
    h.greet()
    Hello // Force initialization of constructor
    println("Finished all breakpoints")
  }
  class Hello {
    def greet(): Unit = {
      println("Breakpoint in hello class")
      class InnerHello { println("Breakpoint in hello inner class") }
      new InnerHello()
      ()
    }
  }
  object Hello {
    println("Breakpoint in hello object")
    val a = 1
  }
}
