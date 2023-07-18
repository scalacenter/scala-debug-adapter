package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class ScalaStackTraceTests extends DebugTestSuite {
  val scalaVersion = ScalaVersion.`3.1+`

  test("2 lists of args") {
    val source =
      """|package example
         |def m(t: Int)(s: String) = {
         |  println(s)
         |}
         |object Main:
         |  def main(args: Array[String]): Unit =
         |    m(42)("")
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        3,
        List(
          "example.m(t: Int)(s: String): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

  test("anon function") {
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

  test("trait initializer ") {
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
        List(
          "example.m2(t: Int): Int",
          "example.m1.<default 3>(y: Int): Int",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

  test("lazy val") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |
         |    def m(t: Int) = {
         |      lazy val m1 : Int = {
         |        def m(t: Int): Int = {
         |          t + 1
         |        }
         |        m(2)
         |      }
         |      m1
         |    }
         |    m(4)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        8,
        List(
          "Main.main.m.m1.m(t: Int): Int",
          "Main.main.m.m1: Int",
          "Main.main.m(t: Int): Int",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

  test("local class") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    class A {
         |      def m2( t : String ) : Unit  = {
         |        println(t)
         |      }
         |    }
         |    A().m2("hello")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)

    check(
      Breakpoint(
        6,
        List(
          "Main.main.A.m2(t: String): Unit",
          "Main.main(args: Array[String]): Unit"
        )
      )
    )
  }

}
