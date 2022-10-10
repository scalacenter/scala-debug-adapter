package ch.epfl.scala.debugadapter.internal.scalasig

import utest._
import ch.epfl.scala.debugadapter.MainDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.NoopLogger

object Scala212DecompilerTests extends DecompilerSuite(ScalaVersion.`2.12`)
object Scala213DecompilerTests extends DecompilerSuite(ScalaVersion.`2.13`)

abstract class DecompilerSuite(scalaVersion: ScalaVersion) extends TestSuite {
  override val tests: Tests = Tests {
    "cannot decompile local methods and classes" - {
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

      val debuggee = MainDebuggee.mainClassRunner(source, "", scalaVersion)

      val scalaSig = decompile(debuggee, "example/A.class")
      val methods = scalaSig.entries.collect { case m: MethodSymbol => m }
      assert(methods.size == 2) // init and m

      assertNoScalaSig(debuggee, "example/A$B$1$.class")
      assertNoScalaSig(debuggee, "example/A$C$1.class")
    }

    "decompiles a lazy field" - {
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
      val debuggee = MainDebuggee.mainClassRunner(source, "", scalaVersion)

      val scalaSigA = decompile(debuggee, "example/A.class")
      val methodsA = scalaSigA.entries.collect { case m: MethodSymbol => m }
      assert(methodsA.size == 2) // init and a

      val scalaSigB = decompile(debuggee, "example/B.class")
      val methodsB = scalaSigB.entries.collect { case m: MethodSymbol => m }
      assert(methodsB.size == 2) // init and b
    }

    "decompiles a case class" - {
      val source =
        """|package example
           |case class A(a: String)
           |""".stripMargin

      val debuggee = MainDebuggee.mainClassRunner(source, "", scalaVersion)

      val scalaSig = decompile(debuggee, "example/A.class")
      val methods =
        scalaSig.entries
          .collect { case m: MethodSymbol => m }
          .filter(m => m.isMethod)
          .toSeq

      val (methodsOfObject, methodsOfClass) =
        methods.partition(m => m.parent.get.isModule)

      val (syntheticMethods, nonSyntheticMethods) = methodsOfClass.partition(m => m.isSynthetic)
      // init and getter
      assert(nonSyntheticMethods.size == 2)

      // copy, toString, equals, hashCode, productArity...
      val expected = if (scalaVersion.isScala213) 11 else 10
      assert(syntheticMethods.size == expected)

      // init, apply, unapply, toString and writeReplace
      assert(methodsOfObject.size == 5)

      assertNoScalaSig(debuggee, "example/A$.class")
    }
  }

  private def decompile(debuggee: MainDebuggee, classFile: String): ScalaSig = {
    val classBytes = debuggee.mainModule.readBytes(classFile)
    val scalaSig = Decompiler.decompile(classBytes, classFile, NoopLogger)
    assert(scalaSig.isDefined)
    scalaSig.get
  }

  private def assertNoScalaSig(
      debuggee: MainDebuggee,
      classFile: String
  ): Unit = {
    val classBytes = debuggee.mainModule.readBytes(classFile)
    val scalaSig = Decompiler.decompile(classBytes, classFile, NoopLogger)
    assert(scalaSig.isEmpty)
  }

  // only used for debugging
  private def info(entry: Entry): String = {
    entry match {
      case sym: Symbol => info(sym)
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
      case MethodType(resultType, paramRefs) =>
        s"MethodType(${info(resultType.get)}, ${paramRefs.map(r => info(r.get)).mkString("[", ", ", "]")})"
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
