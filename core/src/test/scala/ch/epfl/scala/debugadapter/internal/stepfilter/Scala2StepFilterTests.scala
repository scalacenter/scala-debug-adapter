package ch.epfl.scala.debugadapter.internal.stepfilter

import utest._
import ch.epfl.scala.debugadapter.MainDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSig
import ch.epfl.scala.debugadapter.NoopLogger
import ch.epfl.scala.debugadapter.internal.scalasig._
import ch.epfl.scala.debugadapter.PrintLogger
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider

object Scala213StepFilterTests extends Scala2StepFilterTests(ScalaVersion.`2.13`)
object Scala212StepFilterTests extends Scala2StepFilterTests(ScalaVersion.`2.12`)

abstract class Scala2StepFilterTests(scalaVersion: ScalaVersion) extends TestSuite {
  def isScala213: Boolean = scalaVersion.isScala213

  override def tests: Tests = Tests {
    "extract result types of all kind" - {
      val source =
        """|package example
           |
           |class annot
           |  extends scala.annotation.Annotation
           |  with scala.annotation.StaticAnnotation
           |
           |trait A {
           |  class B
           |}
           |
           |object Main extends A {
           |  class B
           |  def m(a : example.A): example.A = a
           |  def mbis(b: A#B): A#B = b
           |  def mbis(a: A)(b: a.B): a.B = b
           |  def m(a: this.type): this.type = a
           |  def mter(b: Main.super[A].B): Main.super[A].B = b
           |  def mbis(a: A { def b: B }): A { def b: B } = a
           |  def m(x: String @annot): String @annot = x
           |  def m(x: Either[Int, X] forSome { type X }): Either[Y, Int] forSome { type Y } = x.swap
           |  def m[T](x: T): T = x
           |  def mbis(a: Main.type): Main.type = a
           |}
           |""".stripMargin
      val debuggee = MainDebuggee.mainClassRunner(source, "", scalaVersion)
      val stepFilter = new Scala2StepFilter(null, scalaVersion, NoopLogger, testMode = true)

      val scalaSig = decompile(debuggee, "example/Main.class")
      val methods = scalaSig.entries
        .collect { case m: MethodSymbol => m }
        .filter(m => m.isMethod)
      val returnTypes = methods
        .map(m => stepFilter.extractParametersAndReturnType(m.infoType)._2)

      val returnThisType = returnTypes.find(_.isInstanceOf[ThisType])
      assert(returnThisType.nonEmpty)

      // there is no SuperType
      val returnSuperType = returnTypes.find(_.toString.contains("SuperType"))
      assert(returnSuperType.isEmpty)

      val returnSingleType = returnTypes.find(_.isInstanceOf[SingleType])
      assert(returnSingleType.nonEmpty)

      val returnTypeRefType = returnTypes.find(_.isInstanceOf[TypeRefType])
      assert(returnTypeRefType.nonEmpty)

      val returnRefinedType = returnTypes.find(_.isInstanceOf[RefinedType])
      assert(returnRefinedType.nonEmpty)

      val returnAnnotatedType = returnTypes.find(_.isInstanceOf[AnnotatedType])
      assert(returnAnnotatedType.nonEmpty)

      val returnExistentialType =
        returnTypes.find(_.isInstanceOf[ExistentialType])
      assert(returnExistentialType.nonEmpty)
    }

    "extract constant result type (Scala 2.13 only)" - {
      if (isScala213) {
        val source =
          """|package example
             |
             |object Main {
             |  def m(x: "a"): 1 = 1
             |}
             |""".stripMargin
        val debuggee = MainDebuggee.mainClassRunner(source, "", scalaVersion)
        val stepFilter = new Scala2StepFilter(null, scalaVersion, NoopLogger, testMode = true)

        val scalaSig = decompile(debuggee, "example/Main.class")
        val method = scalaSig.entries
          .collect { case m: MethodSymbol => m }
          .filter(m => m.isMethod)
          .find(_.name == "m")
          .get
        val returnType =
          stepFilter.extractParametersAndReturnType(method.infoType)._2
        assert(returnType.isInstanceOf[ConstantType])
      }
    }

    "all Java types are known by the class loader" - {
      val debuggee = MainDebuggee.mainClassRunner("", "", scalaVersion)
      val sourceLookUp = SourceLookUpProvider(debuggee.classEntries, PrintLogger)
      val stepFilter = new Scala2StepFilter(sourceLookUp, scalaVersion, NoopLogger, testMode = true)

      stepFilter.scalaAliasesToJavaTypes.values.foreach { javaClass =>
        assert(sourceLookUp.containsClass(javaClass))
      }
    }
  }

  private def decompile(debuggee: MainDebuggee, classFile: String): ScalaSig = {
    val classBytes = debuggee.mainModule.readBytes(classFile)
    val scalaSig = Decompiler.decompile(classBytes, classFile, NoopLogger)
    assert(scalaSig.isDefined)
    scalaSig.get
  }
}
