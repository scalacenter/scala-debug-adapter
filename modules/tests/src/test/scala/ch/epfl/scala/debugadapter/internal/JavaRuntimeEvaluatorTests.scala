package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.DebugStepAssert
import ch.epfl.scala.debugadapter.testfmk.Evaluation
import ch.epfl.scala.debugadapter.DebugConfig
import ch.epfl.scala.debugadapter.testfmk.ObjectRef

object JavaRuntimeEvaluatorEnvironments {
  val localVarTestSource =
    """|package example;
       |
       |public class Main {
       |  public static void main(String[] args) {
       |    int i = 0;
       |    int j = 1;
       |    int k = 2;
       |    int l = 3;
       |    Main main = new Main();
       |    int x = 1+1;
       |  }
       |}
       |""".stripMargin

  val fieldMethodSource =
    """|package example;
       |
       |public class Main {
       |  private static String coucou = "coucou";
       |  private String lapin = "lapin";
       |  public String love = "love";
       |
       |  public static void main(String[] args) {
       |    Foo foo = new Foo(); SuperFoo hiddenFoo = new Foo();
       |    SuperFoo superfoo = new SuperFoo();
       |    Main main = new Main();
       |    System.out.println("the end");
       |    main.foo();
       |  }
       |
       |  public void foo() {
       |    System.out.println("foo");
       |  }
       |
       |  public static String staticMethod() { return "i am static"; }
       |}
       |
       |class SuperFoo { 
       |  private String superfoo = "hello super";
       |  public boolean superbar = true;
       |  public static String foofoo = "superfoofoo";
       |
       |  public static String staticMethod() { return "i am static superfoo"; }
       |}
       |
       |class Foo extends SuperFoo {
       |  public static String foofoo = "foofoo";
       |  private String lapinou = "lapinou";
       |
       |  public static String staticMethod() { return "i am static foo"; }
       |}
       |""".stripMargin

  val nested =
    """|package example;
       |
       |public class Main {
       |  public static void main(String[] args) {
       |    Foo foo = new Foo();
       |    System.out.println(Foo.StaticFriendFoo.z);
       |    System.out.println(Foo.FriendFoo.z);
       |    Main main = new Main();
       |    Inner inner = main.foo();
       |    System.out.println(StaticInner.z); System.out.println(StaticInner.StaticDoubleInner.z);
       |    System.out.println("the end");
       |  }
       |
       |  public Inner foo() {
       |    return new Inner(42);
       |  }
       |
       |  class Inner {
       |    public static final int z = 42;
       |    public static final String helloInner = "hello inner";
       |    public int x;
       |    public int y;
       |    public Inner(int x) {
       |      this.x = x;
       |      this.y = x + 1;
       |    }
       |    public String helloInner() { return "hello inner " + x; }
       |
       |    public String nonStaticMethod() { return "i am non static inner"; }
       |  }
       |
       |  static class StaticInner {
       |    public static int z = 84;
       |    public static class StaticDoubleInner {
       |      public static int z = 168;
       |    }
       |    public static String staticMethod() { return "i am static static_inner"; }
       |  }
       |}
       |
       |class Foo {
       |  public FriendFoo friendFoo;
       |  public Foo() {
       |    this.friendFoo = new FriendFoo(this);
       |  }
       |  
       |  public FriendFoo friendFoo(Foo f) { return new FriendFoo(f); }
       |  class FriendFoo {
       |    public Foo f;
       |    public int y = 42;
       |    public static final int z = 43;
       |    public static final String greet = "Friendly";
       |    
       |    public FriendFoo(Foo f) { this.f = f; }
       |    public String greet() { return "Friend"; }
       |    public int add(int x, int y) { return x + y; }
       |
       |    public String nonStaticMethod() { return "i am non static friend_foo"; }
       |    public static String staticMethod() { return "i am static friend_foo"; }
       |  }
       |  static class StaticFriendFoo {
       |    public static int z = 168;
       |    public static String staticMethod() { return "i am static static_friend_foo"; }
       |  }
       |}
       |
    """.stripMargin
}

class JavaRuntimeEvaluatorTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`
  lazy val localVar =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.localVarTestSource, "example.Main", scalaVersion)
  lazy val fieldMethod =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.fieldMethodSource, "example.Main", scalaVersion)
  lazy val nested =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.nested, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  // TODO: fix bug when trying to evaluate 'l'
  // TODO: operations on primitive types (+...)
  test("should retrieve the value of a local variable from jdi --- java") {
    implicit val debuggee = localVar
    check(
      Breakpoint(10),
      DebugStepAssert.inParallel(
        Evaluation.success("i", 0),
        Evaluation.success("j", 1),
        Evaluation.success("k", 2),
        Evaluation.success("main", ObjectRef("Main"))
      )
    )
  }

  test("Should retrieve the value of a field, should it be private or not --- java") {
    implicit val debuggee = fieldMethod
    check(
      Breakpoint(13),
      DebugStepAssert.inParallel(
        Evaluation.success("main.lapin", "lapin"),
        Evaluation.success("main.love", "love"),
        Evaluation.success("foo.lapinou", "lapinou"),
        Evaluation.success("foo.superbar", true),
        Evaluation.success("foo.superfoo", "hello super"),
        Evaluation.success("Foo.foofoo", "foofoo"),
        Evaluation.success("SuperFoo.foofoo", "superfoofoo"),
        Evaluation.failed("coucou"),
        Evaluation.failed("lapin"),
        Evaluation.failed("love")
      )
    )
  }

  test("Should retrieve the value of a field without selector --- java") {
    implicit val debuggee = fieldMethod
    check(
      Breakpoint(17),
      DebugStepAssert.inParallel(
        Evaluation.failed("coucou", "Accessing static field"),
        Evaluation.success("lapin", "lapin"),
        Evaluation.success("love", "love")
      )
    )
  }

  test("Should call static method --- java") {
    implicit val debuggee = fieldMethod
    check(
      Breakpoint(17),
      DebugStepAssert.inParallel(
        Evaluation.success("Main.staticMethod()", "i am static"),
        Evaluation.success("SuperFoo.staticMethod()", "i am static superfoo"),
        Evaluation.success("Foo.staticMethod()", "i am static foo")
      )
    )
  }

  test("Should get the value of a field in a nested type --- java") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("inner.helloInner()", "hello inner 42"),
        Evaluation.failed("inner.helloInner", "Accessing static field"),
        Evaluation.success("Main.StaticInner.z", 84),
        Evaluation.success("Foo.StaticFriendFoo.z", 168),
        Evaluation.failed("foo.friendFoo(new Foo()).z", "Accessing static field"),
        Evaluation.failed("foo.friendFoo(new Foo()).staticMethod()", "Accessing static method"),
        Evaluation.success("new Foo().friendFoo(new Foo()).y", 42),
        Evaluation.failed("new Foo().friendFoo(new Foo()).greet", "Accessing static field"),
        Evaluation.success("Main.StaticInner.StaticDoubleInner.z", 168)
      )
    )
  }

  test("Should not call static members from instance") {
    implicit val debuggee = fieldMethod
    check(
      Breakpoint(13),
      DebugStepAssert.inParallel(
        Evaluation.failed("main.coucou", "Accessing static field"),
        Evaluation.failed("hiddenFoo.foofoo", "Accessing static field"),
        Evaluation.failed("foo.foofoo", "Accessing static field"),
        Evaluation.failed("superfoo.foofoo", "Accessing static field"),
        Evaluation.failed("main.staticMethod()", "Accessing static method")
      )
    )

  }

  test("Should call a method on an instance of a nested type --- java") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("inner.nonStaticMethod()", "i am non static inner"),
        Evaluation.success("new Foo().friendFoo(new Foo()).nonStaticMethod()", "i am non static friend_foo")
      )
    )
  }

  test("Should call a static method on nested & inner types --- java") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("StaticInner.staticMethod()", "i am static static_inner"),
        Evaluation.failed("Main.Inner.staticMethod()"),
        Evaluation.success("Main.StaticInner.staticMethod()", "i am static static_inner"),
        Evaluation.success("Foo.StaticFriendFoo.staticMethod()", "i am static static_friend_foo")
      )
    )
  }
}
