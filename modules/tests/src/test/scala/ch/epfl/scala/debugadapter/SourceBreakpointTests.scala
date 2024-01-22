package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class Scala213SourceBreakpointTests extends SourceBreakpointTests(ScalaVersion.`2.13`) {
  test("evaluate breakpoint in lambda --- force runtime evaluation") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    List(new java.lang.Integer(1), new java.lang.Integer(2), new java.lang.Integer(3)).map { i => 
         |      val msg = i + 2
         |      println(msg)
         |    }
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee =
      TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly))(
      Breakpoint(6, "i == 2"),
      Evaluation.success("msg", 4),
      Breakpoint(8)
    )
  }
}
class Scala3SourceBreakpointTests extends SourceBreakpointTests(ScalaVersion.`3.3`) {
  test("evaluate breakpoint in lambda --- force runtime evaluation") {
    val source =
      """|package example
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    List(new java.lang.Integer(1), new java.lang.Integer(2), new java.lang.Integer(3)).map { i => 
         |      val msg = i + 2
         |      println(msg)
         |      println(s"$msg bis")
         |    }
         |    println("Hello, World!")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(defaultConfig.copy(evaluationMode = DebugConfig.RuntimeEvaluationOnly))(
      Breakpoint(5, "i == 2"),
      Breakpoint(6, "i == 2"),
      Evaluation.success("i + 2", 4)
    )
  }
}

class SourceBreakpointTests(val scalaVersion: ScalaVersion) extends DebugTestSuite {
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

  test("logpoint interpolation") {
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

  test("plain-text logpoint") {
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
    check(defaultConfig.copy(evaluationMode = DebugConfig.NoEvaluation))(Logpoint(6, "hello world", "hello world"))
  }
}
