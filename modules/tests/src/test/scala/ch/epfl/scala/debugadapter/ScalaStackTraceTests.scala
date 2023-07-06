package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class ScalaStackTraceTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`

  test("should show the correct stackTrace in nested calls") {
    val source =
      """|package example
         |class A {
         |  def method1(b: B): String = b.method2(this)
         |}
         |class B {
         |  def method2(a: A): String = new C().method3(a)
         |}
         |class C {
         |  def method3(a: A): String = new D().method4(a)
         |}
         |class D {
         |  def method4(a: A): String = "Hello, " + a.toString + "!"
         |}
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A()
         |    val b = new B()
         |    val result = a.method1(b)
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        12,
        List(
          "D.method4(a: A): String",
          "C.method3(a: A): String",
          "B.method2(a: A): String",
          "A.method1(b: B): String",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )

  }
  test("should show the correct stackTrace when calling a function with 2 lists of args") {
    val source =
      """|package example
         |class A {
         |  def method1(b1: B)(b2 :B): String = b1.method2(this)
         |}
         |class B {
         |  def method2(a: A): String = new C().method3(a)
         |}
         |class C {
         |  def method3(a: A): String = new D().method4(a)
         |}
         |class D {
         |  def method4(a: A): String = "Hello, " + a.toString + "!"
         |}
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A()
         |    val b = new B()
         |    val result = a.method1(b)(b)
         |    println(result)
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        12,
        List(
          "D.method4(a: A): String",
          "C.method3(a: A): String",
          "B.method2(a: A): String",
          "A.method1(b1: B)(b2: B): String",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )

  }

  test("should show the correctly filtered  stackTrace 1") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    new Hello().greet()
         |
         |  class Hello():
         |    def greet(): Unit =
         |     List(1,2,3).map ( n => {
         |           n+1
         |     })
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(
        10,
        List(
          "Main$Hello.greet$$anonfun$1(int): int",
          "JFunction1$mcII$sp.apply(t: Any): Any",
          "List.map[B](f: A => B): List[B]",
          "Main.Hello.greet(): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      ),
      Breakpoint(10),
      Breakpoint(10)
    )

  }

  test("should show the correct filtered stackTrace 2 ") {
    val source =
      """|package example
         |case class intg(n : Int)
         |object Main:
         |
         |  def main(args: Array[String]): Unit =
         |  
         |    new Hello().greet()
         |    
         |
         |  class Hello():
         |    def greet(): Unit =
         |      Hell().gre()
         |     
         |     
         |
         |    class Hell():
         |        def gre(): Unit = {
         |
         |         println("hello")
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        19,
        List(
          "Main.Hello.Hell.gre(): Unit",
          "Main.Hello.greet(): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )

  }

  test("should show the correct stack trace  with a trait initializer ") {
    val source =
      """|package example
         |trait MyClass(t : Int) {
         |  println(t)
         |}
         |
         |class A extends MyClass(2)
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val myObj = new A()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        3,
        List(
          "MyClass.<init>(t: Int): Unit",
          "A.<init>(): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )

  }

}
