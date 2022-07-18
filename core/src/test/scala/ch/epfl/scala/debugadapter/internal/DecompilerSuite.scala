package ch.epfl.scala.debugadapter.internal

import utest._
import ch.epfl.scala.debugadapter.MainDebuggeeRunner
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.internal.scalasig._

object Scala212DecompilerTests extends DecompilerSuite(ScalaVersion.`2.12`)
object Scala213DecompilerTests extends DecompilerSuite(ScalaVersion.`2.13`)

abstract class DecompilerSuite(scalaVersion: ScalaVersion) extends TestSuite {

  override val tests: Tests = Tests {
    "local methods and classes" - {
      val source =
        """|package example
           |
           |class A {
           |  def m(): String = {
           |    object B {
           |      override def toString(): String = "B"
           |    }
           |    class C {
           |      override def toString(): String = "C"
           |    }
           |    val f: String => String = x => x
           |    f(B.toString + (new C).toString)
           |  }
           |}
           |""".stripMargin

      val runner =
        MainDebuggeeRunner.mainClassRunner(source, "", scalaVersion)

      decompile(runner, "example/A.class") { scalaSig =>
        val methods = scalaSig.entries.collect { case m: MethodSymbol => m }
        assert(methods.size == 2) // init and m
      }

      assertNoScalaSig(runner, "example/A$B$1$.class")
      assertNoScalaSig(runner, "example/A$C$1.class")
    }

    "lazy field" - {
      val source =
        """|package example
           |
           |class A extends B {
           |  lazy val a = "a"
           |}
           |
           |trait B {
           |  lazy val b = "b"
           |}
           |""".stripMargin

      val runner =
        MainDebuggeeRunner.mainClassRunner(source, "", scalaVersion)

      decompile(runner, "example/A.class") { scalaSig =>
        val methods = scalaSig.entries.collect { case m: MethodSymbol => m }
        assert(methods.size == 2) // init and a
      }

      decompile(runner, "example/B.class") { scalaSig =>
        val methods = scalaSig.entries.collect { case m: MethodSymbol => m }
        assert(methods.size == 2) // init and b
      }
    }
  }

  private def decompile(runner: MainDebuggeeRunner, classFile: String)(
      assertion: ScalaSig => Unit
  ): Unit = {
    val classBytes = runner.projectEntry.readBytes(classFile)
    val scalaSig = Decompiler.decompile(classBytes, classFile)
    assert(scalaSig.isDefined)
    assertion(scalaSig.get)
  }

  private def assertNoScalaSig(
      runner: MainDebuggeeRunner,
      classFile: String
  ): Unit = {
    val classBytes = runner.projectEntry.readBytes(classFile)
    val scalaSig = Decompiler.decompile(classBytes, classFile)
    assert(scalaSig.isEmpty)
  }

  // only used for debugging
  private def info(entry: Entry): String = {
    entry match {
      case sym: scalasig.Symbol => info(sym)
      case SymAnnot(symbol, infoRef, annotArgs, named) => "SymAnnot"
      case AnnotInfo => "AnnotInfo"
      case Modifiers(flags, privateWithin) => "Modifiers"
      case Children => "Children"
      case Name(value) => s"Name($value)"
      case ScalaSymbol(value) => s"ScalaSymbol($value)"
      case tpe: Type => info(tpe)
      case AnnotArgArray(args) => s"AnnotArgArray"
      case Constant(value) => s"Constant($value)"
      case Tree => "Tree"
    }
  }

  // only used for debugging
  private def info(symbol: Symbol): String = {
    symbol match {
      case NoSymbol => "NoSymbol"
      case TypeSymbol(info) => s"TypeSymbol(${info.name})"
      case AliasSymbol(info) => s"AliasSymbol(${info.name})"
      case ClassSymbol(info, thisTypeRef) =>
        s"ClassSymbol(${info.name}: ${this.info(info.info.get)}, ${thisTypeRef
            .map(ref => this.info(ref.get))})"
      case ObjectSymbol(info) => s"ObjectSymbol(${info.name})"
      case MethodSymbol(info, _) => s"MethodSymbol(${info.name})"
      case ExternalSymbol(nameRef, ownerRef, isObject) =>
        s"ExternalSymbol(${nameRef.get.value})"
    }
  }

  // only used for debugging
  private def info(tpe: Type): String = {
    tpe match {
      case NoType => "NoType"
      case NoPrefixType => "NoPrefixType"
      case ThisType(symbol) => s"ThisType(${symbol.get.name})"
      case SuperType(typeRef, superTypeRef) =>
        s"SuperType(${info(typeRef.get)}, ${info(superTypeRef.get)})"
      case SingleType(typeRef, symbol) =>
        s"SingleType(${info(typeRef.get)}, ${symbol.get.name})"
      case ConstantType(constant) => s"ConstantType($constant)"
      case TypeRefType(prefix, symbol, typeArgs) =>
        s"TypeRefType(${info(prefix.get)}, ${symbol.get.name}, ${typeArgs
            .map(ref => info(ref.get))
            .mkString("[", ", ", "]")})"
      case TypeBoundsType(lower, upper) => s"TypeBoundsType(???, ???)"
      case RefinedType(classSym, typeRefs) => s"RefinedType(???, ???)"
      case ClassInfoType(symbol, typeRefs) =>
        s"ClassInfoType(${symbol.name}, ${typeRefs.map(ref => info(ref.get)).mkString("[", ",", "]")})"
      case ClassInfoTypeWithCons(symbol, typeRefs, cons) =>
        s"ClassInfoTypeWithCons(???, ???, ???)"
      case MethodType(resultType, paramRefs) => s"MethodType(???, ???)"
      case NullaryMethodType(resultType) => s"NullaryMethodType(???)"
      case PolyType(typeRef, paramRefs) => s"PolyType(???, ???)"
      case PolyTypeWithCons(typeRef, paramRefs, cons) =>
        s"PolyTypeWithCons(???, ???, ???)"
      case ImplicitMethodType(resultType, paramRefs) =>
        s"ImplicitMethodType(???, ???)"
      case AnnotatedType(typeRef) => s"AnnotateType(???)"
      case AnnotatedWithSelfType(typeRef, symbol, attribTreeRefs) =>
        s"AnnotatedWithSelfType(???, ???, ???)"
      case DeBruijnIndexType(typeLevel, typeIndex) =>
        s"DeBruijnIndexType(???, ???)"
      case ExistentialType(typeRef, paramRefs) => s"ExistentialType(???, ???)"
    }
  }
}
