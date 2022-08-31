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

      val termsOfA = stepFilter.extractScalaTerms("example.A")
      assert(termsOfA.size == 2) // <init> and m

      val termsOfB = stepFilter.extractScalaTerms("example.B")
      assert(termsOfB.size == 1) // <init>

      val termsOfMain = stepFilter.extractScalaTerms("example.Main$")
      assert(termsOfMain.size == 3) // main, writeReplace, <init>
    }
  }
