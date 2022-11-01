package ch.epfl.scala.debugadapter.internal.stepfilter

import munit.FunSuite
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Java8
import ch.epfl.scala.debugadapter.Java9OrAbove
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Flags
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee

class ScalaStepFilterBridgeTests extends FunSuite:
  test("should not step into mixin forwarder") {
    val source =
      """|package example
         |
         |trait A {
         |  def m(): String = "A.m()"
         |}
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val b = new B
         |    b.m()
         |  }
         |}
         |
         |class B extends A
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`3.2`)
    val stepFilter = getStepFilter(debuggee)

    val termsOfA = stepFilter.extractScalaTerms("example.A", false)
    assert(termsOfA.size == 2) // <init> and m

    val termsOfB = stepFilter.extractScalaTerms("example.B", false)
    assert(termsOfB.size == 1) // <init>

    val termsOfMain = stepFilter.extractScalaTerms("example.Main$", false)
    assert(termsOfMain.size == 3) // main, writeReplace, <init>
  }

  test("should not step into getters") {
    val source =
      """|package example
         |
         |object Main {
         |  var x = "x"
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.Main", ScalaVersion.`3.2`)
    val stepFilter = getStepFilter(debuggee)

    val terms = stepFilter.extractScalaTerms("example.Main$", false)
    assert(terms.size == 4)

    val getX = terms.find(_.name.toString == "x").get
    assert(!getX.is(Flags.Method))

    val setX = terms.find(_.name.toString == "x_=").get
    assert(setX.isAllOf(Flags.Method | Flags.Accessor))
  }

  test("should step into methods of value classes") {
    val source =
      """|package example
         |
         |class A(val x: String) extends AnyVal {
         |  def m(): String = {
         |    x + x
         |  }
         |}
         |""".stripMargin

    val debuggee = TestingDebuggee.mainClass(source, "example.A", ScalaVersion.`3.2`)
    val stepFilter = getStepFilter(debuggee)

    val objTerms = stepFilter.extractScalaTerms("example.A$", false)
    assert(objTerms.size == 2) // writeReplace and <init>
    // it does not contain the extension method of value class A

    val valueClsTerms = stepFilter.extractScalaTerms("example.A$", true)
    assert(valueClsTerms.size == 5) // hashCode, equals, <init>, m, x
  }

  test("should not step into synthetic methods of case classes") {
    val debuggee = TestingDebuggee.mainClass("", "example.Main", ScalaVersion.`3.2`)
    val stepFilter = getStepFilter(debuggee)

    val objTerms =
      stepFilter.extractScalaTerms("scala.runtime.ScalaRunTime$", false)
    assert(objTerms.size == 31)
  }

  private def getStepFilter(debuggee: Debuggee): ScalaStepFilterBridge = {
    val javaRuntimeJars = debuggee.javaRuntime.toSeq.flatMap {
      case Java8(_, classJars, _) => classJars
      case java9OrAbove: Java9OrAbove =>
        java9OrAbove.classSystems.map(_.fileSystem.getPath("/modules", "java.base"))
    }
    val debuggeeClasspath = debuggee.classPath.toArray ++ javaRuntimeJars
    new ScalaStepFilterBridge(debuggeeClasspath, println, true)
  }
