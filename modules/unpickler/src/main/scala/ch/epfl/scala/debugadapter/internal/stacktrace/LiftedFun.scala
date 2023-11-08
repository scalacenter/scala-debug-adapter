package ch.epfl.scala.debugadapter.internal.stacktrace

import tastyquery.Trees.*
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import tastyquery.Contexts.*
import tastyquery.Types.*

case class LiftedFun[+T](value: T, inlinedFrom: List[InlineMethodApply], inlineArgs: Seq[TermTree]):
  def isInline: Boolean = inlinedFrom.nonEmpty

object LiftedFun:
  extension (tree: LiftedFun[Tree]) def pos: SourcePosition = tree.value.pos

  extension (sym: LiftedFun[TermSymbol])
    def declaredType(using Context): TypeOrMethodic =
      sym.inlinedFrom.foldLeft(sym.value.declaredType)((tpe, inlineApply) => inlineApply.substTypeParams(tpe))

  extension [T](xs: LiftedFun[Seq[T]])
    def traverse: Seq[LiftedFun[T]] =
      xs.value.map(LiftedFun(_, xs.inlinedFrom, xs.inlineArgs))

  def lift[A, B](pf: PartialFunction[A, B]): PartialFunction[LiftedFun[A], LiftedFun[B]] =
    def f(inlined: LiftedFun[A]): Option[LiftedFun[B]] =
      pf.lift(inlined.value).map(LiftedFun(_, inlined.inlinedFrom, inlined.inlineArgs))
    f.unlift
