package ch.epfl.scala.debugadapter.internal.scalasig

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Originally copied from https://github.com/JetBrains/intellij-scala
 * https://github.com/JetBrains/intellij-scala/blob/074e8f98d9789b3e7def3ade8d39e7ae770beccf/scala/decompiler/src/org/jetbrains/plugins/scala/decompiler/scalasig/ScalaSig.scala
 *
 * Nikolay.Tropin
 * 19-Jul-17
 */
class ScalaSig(val entries: Array[Entry]) {
  private var initialized: Boolean = false

  def get(idx: Int): Entry = entries(idx)

  def isInitialized: Boolean = initialized
  def finished(): Unit = initialized = true

  private val classes = ArrayBuffer.empty[ClassSymbol]
  private val objects = ArrayBuffer.empty[ObjectSymbol]
  private val symAnnots = ArrayBuffer.empty[SymAnnot]
  private val parentToChildren = mutable.HashMap.empty[Int, ArrayBuffer[Symbol]]

  def topLevelClasses: Iterable[ClassSymbol] = classes.filter(isTopLevelClass)
  def topLevelObjects: Iterable[ObjectSymbol] = objects.filter(isTopLevel)

  def findCompanionClass(objectSymbol: ObjectSymbol): Option[ClassSymbol] = {
    val owner: Symbol = objectSymbol.symbolInfo.owner.get
    val name = objectSymbol.name
    classes.find(c => c.info.owner.get.eq(owner) && c.name == name)
  }

  def children(symbol: ScalaSigSymbol): Iterable[Symbol] = {
    parentToChildren.keysIterator.find(get(_) eq symbol) match {
      case None => Iterable.empty
      case Some(i) => parentToChildren(i)
    }
  }

  def attributes(symbol: ScalaSigSymbol): Iterable[SymAnnot] = {
    def sameSymbol(ann: SymAnnot) = (ann.symbol.get, symbol) match {
      case (s, t) if s == t => true
      case (m1: MethodSymbol, m2: MethodSymbol) if equiv(m1, m2) => true
      case _ => false
    }
    val forSameSymbol = symAnnots.filter(sameSymbol)
    forSameSymbol.groupBy(_.typeRef).values.map(_.head)
  }

  def addClass(c: ClassSymbol): Unit = classes += c
  def addObject(o: ObjectSymbol): Unit = objects += o
  def addAttribute(a: SymAnnot): Unit = symAnnots += a

  def addChild(parent: Option[Ref[Symbol]], child: Symbol): Unit = {
    parent.foreach { ref =>
      val children =
        parentToChildren.getOrElseUpdate(ref.index, ArrayBuffer.empty)
      children += child
    }
  }

  private def isTopLevel(symbol: Symbol): Boolean = symbol.parent match {
    case Some(_: ExternalSymbol) => true
    case _ => false
  }
  private def isTopLevelClass(symbol: Symbol): Boolean =
    !symbol.isModule && isTopLevel(symbol)

  private def equiv(m1: MethodSymbol, m2: MethodSymbol) = {
    def unwrapType(t: Type) = t match {
      case NullaryMethodType(Ref(tp)) => tp
      case _ => t
    }

    m1.name == m2.name && m1.parent == m2.parent &&
    unwrapType(m1.infoType) == unwrapType(m2.infoType)
  }

  def syntheticSymbols(): Seq[Symbol] =
    parentToChildren.valuesIterator.flatten.filter(_.isSynthetic).toList
}
