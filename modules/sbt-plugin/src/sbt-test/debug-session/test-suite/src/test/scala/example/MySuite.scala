package example

import utest._

object MySuite extends TestSuite {
  def tests: Tests = Tests {
    "should allow breakpoints in both test and main" - {
      println("Breakpoint in test")
      new Hello().greet()
      println("Finished all breakpoints")
    }
  }
}
