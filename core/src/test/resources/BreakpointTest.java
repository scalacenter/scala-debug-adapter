class BreakpointTest {
  public static void main(String[] args) {
    System.out.println("Breakpoint in main method");
    BreakpointTest test = new BreakpointTest();
    test.greet();
    System.out.println("Finished all breakpoints");
  }

  public BreakpointTest() {
    System.out.println("Breakpoin in constructor");
  }

  public void greet() {
    System.out.println("Breakpoint in method");
  }
}
