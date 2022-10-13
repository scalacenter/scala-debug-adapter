package example

import utest._

object MySuite extends TestSuite {
  def tests: Tests = Tests {
    "should allow breakpoints in both test and main" - {
      println("Breakpoint in main method")
      new Hello().greet()
      println("Finished all breakpoints")
    }
  }
}
