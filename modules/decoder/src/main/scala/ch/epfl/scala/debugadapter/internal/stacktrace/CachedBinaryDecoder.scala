package ch.epfl.scala.debugadapter.internal.stacktrace

import ch.epfl.scala.debugadapter.internal.binary
import ch.epfl.scala.debugadapter.internal.binary.Method
import ch.epfl.scala.debugadapter.internal.binary.SignedName
import tastyquery.Contexts.*
import tastyquery.Symbols.*

import scala.collection.concurrent.TrieMap

class CachedBinaryDecoder(using Context, ThrowOrWarn) extends BinaryDecoder:
  private val classCache: TrieMap[String, DecodedClass] = TrieMap.empty
  private val methodCache: TrieMap[(String, SignedName), DecodedMethod] = TrieMap.empty
  private val liftedTreesCache: TrieMap[Symbol, Seq[LiftedTree[?]]] = TrieMap.empty

  override def decode(cls: binary.ClassType): DecodedClass =
    classCache.getOrElseUpdate(cls.name, super.decode(cls))

  override def decode(method: Method): DecodedMethod =
    methodCache.getOrElseUpdate((method.declaringClass.name, method.signedName), super.decode(method))

  override protected def collectAllLiftedTrees(owner: Symbol): Seq[LiftedTree[?]] =
    liftedTreesCache.getOrElseUpdate(owner, super.collectAllLiftedTrees(owner))
