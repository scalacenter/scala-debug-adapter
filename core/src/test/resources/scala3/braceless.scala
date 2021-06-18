package example

object Example:
  def main(args: Array[String]): Unit =
    println("Breakpoint in main method")
    new Hello().greet()
    println("Finished all breakpoints")

  class Hello():
    def greet(): Unit =
      println("Breakpoint in hello class")
