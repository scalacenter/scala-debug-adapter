package ch.epfl.scala.debugadapter.internal.scalasig

import ch.epfl.scala.debugadapter.internal.Errors

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * Originally copied from https://github.com/JetBrains/intellij-scala
 * https://github.com/JetBrains/intellij-scala/blob/074e8f98d9789b3e7def3ade8d39e7ae770beccf/scala/decompiler/src/org/jetbrains/plugins/scala/decompiler/scalasig/Ref.scala
 *
 * Nikolay.Tropin
 * 19-Jul-17
 */
class Ref[T <: Entry: ClassTag](val index: Int)(implicit
    val scalaSig: ScalaSig
) {

  def get: T = {
    val entry = scalaSig.get(index)

    if (!scalaSig.isInitialized)
      throw Errors.frameDecodingFailure("usage of scalaSig entry before initialization")

    val expectedClass = implicitly[ClassTag[T]].runtimeClass
    if (!expectedClass.isInstance(entry)) {
      val expName = expectedClass.getCanonicalName
      val actName = entry.getClass.getCanonicalName
      val message = s"wrong type of reference at index $index, expected: $expName, actual: $actName"
      throw Errors.frameDecodingFailure(message)
    }

    entry.asInstanceOf[T]
  }

  override def toString: String =
    if (!scalaSig.isInitialized) "not initialized"
    else get.toString

  override def equals(obj: scala.Any): Boolean = obj match {
    case r: Ref[_] => r.index == index
    case _ => false
  }

  override def hashCode(): Int = index
}

class MappedRef[T <: Entry: ClassTag, S <: Entry: ClassTag](
    val ref: Ref[T],
    val fun: T => S
)(implicit override val scalaSig: ScalaSig)
    extends Ref[S](ref.index) {

  override def get: S = fun(ref.get)
}

object Ref {
  def to[T <: Entry: ClassTag](index: Int)(implicit scalaSig: ScalaSig) =
    new Ref[T](index)

  def unapply[T <: Entry](ref: Ref[T]): Option[T] = Some(ref.get)

  implicit def unwrap[T <: Entry](ref: Ref[T]): T = ref.get

  implicit def unwrapSeq[T <: Entry](refs: Seq[Ref[T]]): Seq[T] =
    refs.map(_.get)

  implicit def unwrapOption[T <: Entry](ref: Option[Ref[T]]): Option[T] =
    ref.map(_.get)

  implicit class RefOps[T <: Entry: ClassTag](ref: Ref[T]) {
    import ref.scalaSig

    def map[S <: Entry: ClassTag](fun: T => S): Ref[S] =
      new MappedRef[T, S](ref, fun)
  }
}
