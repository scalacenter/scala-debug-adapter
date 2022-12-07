package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class Scala213SourceBreakpointTests extends SourceBreakpointTests(ScalaVersion.`2.13`)
class Scala32SourceBreakpointTests extends SourceBreakpointTests(ScalaVersion.`3.2`)

abstract class SourceBreakpointTests(scalaVersion: ScalaVersion) extends DebugTestSuite {
  test("evaluate simple breakpoint") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = 42
         |    println(x)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5, "x == 42"), Evaluation.success("x", 42))
  }

  test("evaluate breakpoint in lambda") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    List(1, 2, 3).map { i => 
         |      val msg = i.toString
         |      println(msg)
         |    }
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Breakpoint(5, "i == 2"), Evaluation.success("i", 2), Breakpoint(8))
  }

  test("evaluate logpoins with dollar") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val i: Int = 3
         |    
         |    print(i)
         |    ()
         |
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Logpoint(6, "i is $i", "i is 3"))
    check(Logpoint(6, "${i * i}", "9"))
    check(Logpoint(6, "{i * i}", "{i * i}"))
  }

  test("evaluate logpoints") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val list = List(1, 2, 3).map { i => 
         |      val msg = i.toString
         |      msg
         |    }
         |    println(list.mkString)
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(Logpoint(5, "i is $i", "i is 1"), Outputed("i is 2"), Outputed("i is 3"), Outputed("123"))
    check(Logpoint(5, "${i * i}", "1"), Outputed("4"), Outputed("9"), Outputed("123"))
    check(
      Logpoint(
        8,
        """|${list.head}
           |${list.tail.head}""".stripMargin,
        "1"
      ),
      Outputed("2"), // second part of the logpoint
      Outputed("123")
    )
  }
}
