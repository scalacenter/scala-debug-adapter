package scala.tools.nsc.evaluation

import scala.tools.nsc.Global

/** Encoding of symbol names for the IR. */
class JavaEncoding[G <: Global](val global: G) {
  import global._

  def encode(tpe: Type): String =
    tpe.dealiasWiden match {
      case ArrayTypeRef(el) => s"[${binaryName(el)}"
      case TypeRef(_, sym, _) => encode(sym.asType)
      case AnnotatedType(_, t) => encode(t)
    }

  def encode(sym: TypeSymbol): String = {
    /* When compiling Array.scala, the type parameter T is not erased and shows up in method
     * signatures, e.g. `def apply(i: Int): T`. A TypeRef to T is replaced by ObjectReference.
     */
    if (!sym.isClass) "java.lang.Object"
    else if (sym.isPrimitiveValueClass) primitiveName(sym)
    else className(sym)
  }

  private def binaryName(tpe: Type): String =
    tpe match {
      case ArrayTypeRef(el) => s"[${binaryName(el)}"
      case TypeRef(_, sym, _) =>
        if (sym.isPrimitiveValueClass) primitiveBinaryName(sym)
        else classBinaryName(sym)
      case AnnotatedType(_, t) => binaryName(t)
    }

  private def primitiveName(sym: Symbol): String =
    if (sym == definitions.UnitClass) "void"
    else if (sym == definitions.BooleanClass) "boolean"
    else if (sym == definitions.CharClass) "char"
    else if (sym == definitions.ByteClass) "byte"
    else if (sym == definitions.ShortClass) "short"
    else if (sym == definitions.IntClass) "int"
    else if (sym == definitions.LongClass) "long"
    else if (sym == definitions.FloatClass) "float"
    else if (sym == definitions.DoubleClass) "double"
    else throw new Exception(s"Unknown primitive value class $sym")

  private def primitiveBinaryName(sym: Symbol): String =
    if (sym == definitions.BooleanClass) "Z"
    else if (sym == definitions.CharClass) "C"
    else if (sym == definitions.ByteClass) "B"
    else if (sym == definitions.ShortClass) "S"
    else if (sym == definitions.IntClass) "I"
    else if (sym == definitions.LongClass) "J"
    else if (sym == definitions.FloatClass) "F"
    else if (sym == definitions.DoubleClass) "D"
    else throw new Exception(s"Unknown primitive value class $sym")

  private def className(sym: Symbol): String = {
    val sym1 =
      if (sym.isModuleClass && sym.isJavaDefined) sym.linkedClassOfClass
      else sym

    /* Some rewirings:
     * - scala.Nothing to scala.runtime.Nothing$.
     * - scala.Null to scala.runtime.Null$.
     */
    if (sym1 == definitions.NothingClass) "scala.runtime.Nothing$"
    else if (sym1 == definitions.NullClass) "scala.runtime.Null$"
    else sym1.javaClassName
  }

  private def classBinaryName(sym: Symbol): String =
    s"L${className(sym)};"
}