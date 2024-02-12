package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.scalasig.ScalaSig
import ch.epfl.scala.debugadapter.internal.scalasig._
import ch.epfl.scala.debugadapter.internal.SourceLookUpProvider
import ch.epfl.scala.debugadapter.testfmk.NoopLogger
import munit.FunSuite

class Scala213DecoderTests extends Scala2DecoderTests(ScalaVersion.`2.13`)
class Scala212DecoderTests extends Scala2DecoderTests(ScalaVersion.`2.12`)

abstract class Scala2DecoderTests(scalaVersion: ScalaVersion) extends FunSuite {
  def isScala213: Boolean = scalaVersion.isScala213

  test("extract result types of all kind") {
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
    val debuggee = TestingDebuggee.mainClass(source, "", scalaVersion)
    val decoder = new Scala2Decoder(null, scalaVersion, NoopLogger, testMode = true)

    val scalaSig = decompile(debuggee, "example/Main.class")
    val methods = scalaSig.entries
      .collect { case m: MethodSymbol => m }
      .filter(m => m.isMethod)
    val returnTypes = methods
      .map(m => decoder.extractParametersAndReturnType(m.infoType)._2)

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

    val returnExistentialType = returnTypes.find(_.isInstanceOf[ExistentialType])
    assert(returnExistentialType.nonEmpty)
  }

  test("extract constant result type (Scala 2.13 only)") {
    assume(isScala213)
    val source =
      """|package example
         |
         |object Main {
         |  def m(x: "a"): 1 = 1
         |}
         |""".stripMargin
    val debuggee = TestingDebuggee.mainClass(source, "", scalaVersion)
    val decoder = new Scala2Decoder(null, scalaVersion, NoopLogger, testMode = true)

    val scalaSig = decompile(debuggee, "example/Main.class")
    val method = scalaSig.entries
      .collect { case m: MethodSymbol => m }
      .filter(m => m.isMethod)
      .find(_.name == "m")
      .get
    val returnType = decoder.extractParametersAndReturnType(method.infoType)._2
    assert(returnType.isInstanceOf[ConstantType])
  }

  test("all Java types are known by the class loader") {
    val debuggee = TestingDebuggee.mainClass("", "", scalaVersion)
    val sourceLookUp = SourceLookUpProvider(debuggee.classEntries, NoopLogger)
    val decoder = new Scala2Decoder(sourceLookUp, scalaVersion, NoopLogger, testMode = true)

    decoder.scalaAliasesToJavaTypes.values.foreach { javaClass =>
      assert(sourceLookUp.containsClass(javaClass))
    }
  }

  private def decompile(debuggee: TestingDebuggee, classFile: String): ScalaSig = {
    val classBytes = debuggee.mainModule.readBytes(classFile)
    val scalaSig = Decompiler.decompile(classBytes, classFile, NoopLogger)
    assert(scalaSig.isDefined)
    scalaSig.get
  }
}
