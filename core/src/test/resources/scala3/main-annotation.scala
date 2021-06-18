package example

@main def app: Unit =
  println("Breakpoint in main method")
  new Hello().greet()
  println("Finished all breakpoints")

class Hello():
  def greet(): Unit =
    println("Breakpoint in hello class")
