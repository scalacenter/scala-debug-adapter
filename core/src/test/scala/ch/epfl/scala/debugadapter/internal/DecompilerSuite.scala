package ch.epfl.scala.debugadapter.internal

import utest._
import ch.epfl.scala.debugadapter.MainDebuggeeRunner
import ch.epfl.scala.debugadapter.ScalaVersion

object DecompilerSuite extends TestSuite {

  // Getters
  override val tests: Tests = Tests {

    "should decompile my class" - {

      val source = """|
                      |package ex
                      |
                      |trait A {
                      |  def foo(): String = {
                      |   val x: Int = 0
                      |   return x.asInstanceOf[String]
                      | }
                      |}
                      |
                      |class B () extends A {
                      |   def foo(i: Int): String = "Hello"
                      |}
                      |
                      |object Main2 {
                      |  def main(args: Array[String]): Unit = {
                      |    val b = new B
                      |    println(b.foo())
                      |  }
                      |}
                      |""".stripMargin

      val runner = MainDebuggeeRunner.mainClassRunner(
        source,
        "",
        ScalaVersion.`2.12`
      )

      val bytes = runner.projectEntry.readBytes("ex/B.class")

      println(bytes.size)
      val text =
        for (b <- bytes)
          yield (Decompiler.sourceNameAndText("", "", b))

      for (t <- text) print(t)
    }
  }
}
