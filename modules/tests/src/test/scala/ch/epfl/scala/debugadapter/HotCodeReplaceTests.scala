package ch.epfl.scala.debugadapter
import ch.epfl.scala.debugadapter.testfmk.DebugTestSuite
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.testfmk.Breakpoint
import ch.epfl.scala.debugadapter.testfmk.Outputed
import ch.epfl.scala.debugadapter.testfmk.Custom
import ch.epfl.scala.debugadapter.testfmk.RedefineClasses

class HotCodeReplaceTests extends DebugTestSuite {
  test("hot test") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit =
         |    new A().m()
         |}
         |
         |class A {
         |  def m() = {
         |    println("A")
         |    println("B")
         |  }
         |}
         |""".stripMargin
    val newSource =
      """|package example
         |
         |class A {
         |  def m() = {
         |    println("C")
         |  }
         |}
         |""".stripMargin
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`3.1+`)
    check(
      Outputed("A"),
      Breakpoint(11),
      Custom {
        debuggee.compileScala(newSource, "hotCodeReloadTest")
        debuggee.classesToUpdate.onNext(Seq("example.A"))
      },
      RedefineClasses(),
      Outputed("C")
    )
  }
}
