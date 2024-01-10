package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class ScalaStackTraceTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`

  test("anonfun") {
    val source =
      """|package example
         |
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    val a = new A
         |    a.m
         |
         |  class A:
         |    def m: Seq[Int] =
         |      List(1,2,3).map { n =>
         |        println("")
         |        n+1
         |      }
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(
        11,
        Seq(
          "Main.A.m.<anon fun>(n: Int): Int",
          "JFunction1$mcII$sp.apply(t: Any): Any",
          "List.map[B](f: A => B): List[B]",
          "Main.A.m: Seq[Int]",
          "Main.main(args: Array[String]): Unit"
        )
      ),
      Breakpoint(11),
      Breakpoint(11)
    )

  }

  test("trait initializer") {
    val source =
      """|package example
         |
         |trait A {
         |  println("Hello")
         |}
         |
         |class B extends A
         |
         |object Main {
         |  def main(args: Array[String]): Unit =
         |    new B
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        4,
        Seq(
          "A.<init>(): Unit",
          "B.<init>(): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )

  }

  test("default values") {
    val source =
      """|package example
         |def m1(y : Int)(z : Int , x: Int = m2(y)): Int = {
         |  x * 2
         |}
         |
         |def m2(t : Int ) : Int = {
         |  t*2  
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    println(m1(2)(3))
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        7,
        Seq(
          "example.m2(t: Int): Int",
          "example.m1.<default 3>(y: Int): Int",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

  test("local method and lazy val") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = 1
         |    lazy val y : Int = {
         |      x + 1
         |    }
         |    def m(z: Int) = x + y + z
         |    m(1)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        6,
        List(
          "Main.main.y.<lazy init>: Int",
          "Main.main.y: Int",
          "Main.main.m(z: Int): Int",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

  test("local class") {
    val source =
      """|package example
         |object Main :
         |  def main(args: Array[String]): Unit = 
         |    class B :
         |      def m() = {
         |        println("hello")
         |      }
         |
         |    class C  extends B 
         |    C().m()
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(
        6,
        Seq(
          "Main.main.B.m(): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

  test("i535") {
    assume(isJava8)
    val source =
      """|package example
         |
         |import java.lang.ref.ReferenceQueue
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    new ReferenceQueue[Any]()
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      Breakpoint(7),
      StepIn.method("ReferenceQueue.<init>(): void"),
      StepOver.method("ReferenceQueue.<init>(): void"),
      StepIn.method("ReferenceQueue$Lock.<init>(ReferenceQueue$1): void")
    )
  }
}
