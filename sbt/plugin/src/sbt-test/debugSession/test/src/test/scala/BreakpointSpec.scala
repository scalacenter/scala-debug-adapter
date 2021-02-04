import utest._

object BreakpointSpec extends TestSuite {
  def tests: Tests = Tests {
    "should allow breakpoints in both test and main" - {
      println("Breakpoint in main method")
      val h = new Hello
      h.greet()
      Hello // Force initialization of constructor
      println("Finished all breakpoints")
    }
  }
}