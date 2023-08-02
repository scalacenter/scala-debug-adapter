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
       |  }
       |  static class StaticFriendFoo {
       |    public static int z = 168;
       |    public static String staticMethod() { return "i am static static_friend_foo"; }
       |  }
       |}
       |
    """.stripMargin

  val preEvaluation =
    """|package example;
       |
       |public class Main {
       |  public static void main(String[] args) {
       |    Test testA = new A();
       |    Test testB = new B();
       |    Test aa = new AA();
       |    System.out.println("ok");
       |  }
       |
       |  public static String foo(Test t) { return t.test(); }
       |  public static String foo(SubTestA t) { return t.a(); }
       |  public static String foo(Test t, SubTestA a) { return t.test(a.a()); }
       |  public static String foo(SubTestB b, SubTestA a) { return b.b() + " " + a.a(); }
       |  public static String foo(SubA aa, Test t) { return aa.aa() + " " + t.test(); }
       |}
       |
       |interface Test {
       |  default String test() {
       |    return "test";
       |  }
       |  default String test(String s) {
       |    return "test " + s;
       |  }
       |}
       |
       |interface SubTestA extends Test {
       |  String a();
       |}
       |
       |interface SubA extends SubTestA {
       |  String aa();
       |}
       |
       |interface SubTestB extends Test {
       |  String b();
       |}
       |
       |abstract class TestImpl implements Test {
       |}
       |
       |class A extends TestImpl implements SubTestA {
       |  public String a() {
       |    return "a";
       |  }
       |}
       |
       |class AA extends A implements SubA {
       |  public String aa() {
       |    return "2a";
       |  }
       |}
       |
       |class B extends TestImpl implements SubTestB {
       |  public String b() {
       |    return "b";
       |  }
       |}
       |""".stripMargin
}

class JavaRuntimeEvaluatorTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`
  lazy val localVar =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.localVarTestSource, "example.Main", scalaVersion)
  lazy val fieldMethod =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.fieldMethodSource, "example.Main", scalaVersion)
  lazy val nested =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.nested, "example.Main", scalaVersion)
  lazy val preEvaluation =
    TestingDebuggee.fromJavaSource(JavaRuntimeEvaluatorEnvironments.preEvaluation, "example.Main", scalaVersion)

  protected override def defaultConfig: DebugConfig =
    super.defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly)

  test("should retrieve the value of a local variable from jdi") {
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

  test("Should retrieve the value of a field, should it be private or not") {
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
        Evaluation.success("coucou", "coucou"),
        Evaluation.failed("lapin"),
        Evaluation.failed("love")
      )
    )
  }

  test("Should retrieve the value of a field without selector") {
    implicit val debuggee = fieldMethod
    check(
      Breakpoint(17),
      DebugStepAssert.inParallel(
        Evaluation.failed("coucou"),
        Evaluation.success("lapin", "lapin"),
        Evaluation.success("love", "love")
      )
    )
  }

  test("Should call static method") {
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

  test("Should get the value of a field in a nested type") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("inner.helloInner()", "hello inner 42"),
        Evaluation.failed("inner.helloInner"),
        Evaluation.success("Main.StaticInner.z", 84),
        Evaluation.success("Foo.StaticFriendFoo.z", 168),
        Evaluation.failed("foo.friendFoo(new Foo()).z"),
        Evaluation.failed("foo.friendFoo(new Foo()).staticMethod()"),
        Evaluation.success("new Foo().friendFoo(new Foo()).y", 42),
        Evaluation.failed("new Foo().friendFoo(new Foo()).greet"),
        Evaluation.success("Main.StaticInner.StaticDoubleInner.z", 168)
      )
    )
  }

  test("Should not call static members from instance") {
    implicit val debuggee = fieldMethod
    check(
      Breakpoint(13),
      DebugStepAssert.inParallel(
        Evaluation.failed("main.coucou"),
        Evaluation.failed("hiddenFoo.foofoo"),
        Evaluation.failed("foo.foofoo"),
        Evaluation.failed("superfoo.foofoo"),
        Evaluation.failed("main.staticMethod()")
      )
    )

  }

  test("Should call a method on an instance of a nested type") {
    implicit val debuggee = nested
    check(
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("inner.nonStaticMethod()", "i am non static inner"),
        Evaluation.success("new Foo().friendFoo(new Foo()).nonStaticMethod()", "i am non static friend_foo")
      )
    )
  }

  test("Should call a static method on nested & inner types") {
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

  test("Should pre evaluate method and resolve most precise method") {
    implicit val debuggee = preEvaluation
    check(
      Breakpoint(8),
      DebugStepAssert.inParallel(
        Evaluation.success("testA.a", "a"),
        Evaluation.success("testB.b", "b"),
        Evaluation.success("Main.foo(testB)", "test"),
        Evaluation.success("Main.foo(testA)", "a"),
        Evaluation.success("Main.foo(testB, testA)", "b a"),
        Evaluation.success("Main.foo(aa, testB)", "2a test"),
        Evaluation.failed("Main.foo(aa, testA)")
      )
    )
  }

  test("Should instantiate inner classes") {
    implicit val debuggee = nested
    check(
      Breakpoint(15),
      Evaluation.success("new Inner(2)", ObjectRef("Main$Inner")),
      Breakpoint(11),
      DebugStepAssert.inParallel(
        Evaluation.success("new main.Inner(2)", ObjectRef("Main$Inner")),
        Evaluation.success("new foo.FriendFoo(foo)", ObjectRef("Foo$FriendFoo"))
      )
    )
  }

  test("Should evaluate static members from a static context") {
    val source =
      """|package example;
         |
         |public class Main {
         |  public static int i = 5;
         |  public static void main(String[] args) {
         |    System.out.println(i);
         |  }
         |  public static String foo() { return "foo"; }
         |  static class Bar {
         |    public int x = 42;
         |    public static String bar() { return "bar"; }
         |    public Bar(int x) { this.x = x; }
         |  } 
         |}""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.fromJavaSource(source, "example.Main", scalaVersion)
    check(
      Breakpoint(6),
      Evaluation.success("i", 5),
      Evaluation.success("foo()", "foo"),
      Evaluation.success("new Bar(42)", ObjectRef("Main$Bar")),
      Evaluation.success("new Bar(42).x", 42),
      Evaluation.success("Bar.bar()", "bar")
    )
  }
}
