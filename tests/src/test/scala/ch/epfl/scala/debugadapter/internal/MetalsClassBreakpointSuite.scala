package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.testfmk.NoopLogger
import munit.FunSuite

class MetalsClassBreakpointSuite extends FunSuite {
  test("simple") {
    check(
      """|package a
         |object B{
         |  class C
         |>>  println(0)
         |}
         |""".stripMargin,
      "a.B$"
    )
  }

  test("apply") {
    check(
      """|package a
         |object Bar {
         |  def apply(): Boolean = {
         |>>  true
         |  }
         |}
         |""".stripMargin,
      "a.Bar$"
    )
  }

  test("nested") {
    check(
      """|package a
         |object Bar {
         |  class Foo{
         |    def apply(): Boolean = {
         |>>    true
         |    }
         |  }
         |}
         |""".stripMargin,
      "a.Bar$Foo"
    )
  }

  test("nested-object") {
    check(
      """|package a
         |object Bar {
         |  object Foo{
         |    def apply(): Boolean = {
         |>>    true
         |    }
         |  }
         |}
         |""".stripMargin,
      "a.Bar$Foo$"
    )
  }

  test("no-package") {
    check(
      """|
         |class B{
         |  class C
         |>>  println(0)
         |}
         |""".stripMargin,
      "B"
    )
  }

  test("method") {
    check(
      """|package a.b
         |class B{
         |  class C
         |  def method() = {
         |    >>  println(0)
         |  }
         |}
         |""".stripMargin,
      "a.b.B"
    )
  }

  test("trait") {
    check(
      """|package a.b
         |trait B{
         |  class C
         |  def method() = {
         |    >>  println(0)
         |  }
         |}
         |""".stripMargin,
      "a.b.B"
    )
  }

  test("package-object") {
    check(
      """|package a.b
         |package object c{
         |  def method() = {
         |    >>  println(0)
         |  }
         |}
         |""".stripMargin,
      "a.b.c.package$"
    )
  }

  test("method-scala3") {
    check(
      """|package a.b
         |def method() = {
         |>>  println(0)
         |}
         |""".stripMargin,
      "a.b.Main$package$",
      scalaVersion = ScalaVersion.`3.0`
    )
  }

  test("inner-class-scala3") {
    check(
      """|package a
         |
         |@main 
         |def helloWorld(): Unit = {
         |  object Even {
         |>>  def unapply(s: String): Boolean = s.size % 2 == 0
         |  }
         |}
         |
         |""".stripMargin,
      "a.Main$package$Even$1$",
      scalaVersion = ScalaVersion.`3.0`
    )
  }

  test("optional-braces") {
    check(
      """|package a
         |
         |@main 
         |def hello(): Unit = 
         |  greet("Alice")
         |  greet("Bob")
         |  System.exit(0)
         |
         |def greet(name: String) = 
         |  val message = s"Hello, $name!"
         |>>println(message)
         |
         |""".stripMargin,
      "a.Main$package$",
      scalaVersion = ScalaVersion.`3.0`
    )
  }

  def check(
      original: String,
      expectedClassName: String,
      scalaVersion: ScalaVersion = ScalaVersion.`2.13`
  ): Unit = {
    val source = original.replace(">>", "  ")
    val lineNumber =
      original.linesIterator.toSeq.indexWhere(_.contains(">>")) + 1

    val debuggee = TestingDebuggee.mainClass(source, "Main", scalaVersion)
    val lookUp = ClassEntryLookUp(debuggee.mainModule, NoopLogger)

    val sourceFile = debuggee.sourceFiles.head.toUri

    val className =
      lookUp.getFullyQualifiedClassName(sourceFile, lineNumber)
    assert(className.contains(expectedClassName))
  }
}
