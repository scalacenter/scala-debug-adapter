package ch.epfl.scala.debugadapter.internal.stepfilter

import utest.*
import ch.epfl.scala.debugadapter.ScalaInstanceCache
import ch.epfl.scala.debugadapter.ScalaVersion
import tastyquery.jdk.ClasspathLoaders.FileKind
import tastyquery.Contexts
import tastyquery.Contexts.Context
import tastyquery.jdk.ClasspathLoaders
import tastyquery.ast.Symbols.PackageClassSymbol
import tastyquery.ast.Names.SimpleName
import tastyquery.ast.Symbols.DeclaringSymbol
import tastyquery.ast.Names.TypeName
import tastyquery.ast.Flags

object StepFilterBridgeTests extends TestSuite:
  override def tests: Tests = Tests {
    "should not step into mixin forwarder" - {
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
      val classpath = ScalaInstanceCache.compile(source, ScalaVersion.`3.1`)
      val stepFilter = new StepFilterBridge(classpath.toArray, println, true)

      val termsOfA = stepFilter.extractScalaTerms("example.A", false)
      assert(termsOfA.size == 2) // <init> and m

      val termsOfB = stepFilter.extractScalaTerms("example.B", false)
      assert(termsOfB.size == 1) // <init>

      val termsOfMain = stepFilter.extractScalaTerms("example.Main$", false)
      assert(termsOfMain.size == 3) // main, writeReplace, <init>
    }

    "should not step into getters" - {
      val source =
        """|package example
           |
           |object Main {
           |  var x = "x"
           |}
           |""".stripMargin

      val classpath = ScalaInstanceCache.compile(source, ScalaVersion.`3.1`)
      val stepFilter = new StepFilterBridge(classpath.toArray, println, true)

      val terms = stepFilter.extractScalaTerms("example.Main$", false)
      assert(terms.size == 4)

      val getX = terms.find(_.name.toString == "x").get
      assert(!getX.is(Flags.Method))

      val setX = terms.find(_.name.toString == "x_=").get
      assert(setX.isAllOf(Flags.Method | Flags.Accessor))
    }

    "should step into methods of value classes" - {
      val source =
        """|package example
           |
           |class A(val x: String) extends AnyVal {
           |  def m(): String = {
           |    x + x
           |  }
           |}
           |""".stripMargin

      val classpath = ScalaInstanceCache.compile(source, ScalaVersion.`3.1`)
      val stepFilter = new StepFilterBridge(classpath.toArray, println, true)

      val objTerms = stepFilter.extractScalaTerms("example.A$", false)
      assert(objTerms.size == 2) // writeReplace and <init>
      // it does not contain the extension method of value class A

      val valueClsTerms = stepFilter.extractScalaTerms("example.A$", true)
      assert(valueClsTerms.size == 5) // hashCode, equals, <init>, m, x
    }
  }
