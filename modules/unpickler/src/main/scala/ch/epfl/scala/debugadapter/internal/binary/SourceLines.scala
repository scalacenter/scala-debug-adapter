package ch.epfl.scala.debugadapter.internal.binary

final case class SourceLines(sourceName: String, lines: Seq[Int]):
  def span: Seq[Int] =
    if lines.size > 2 then Seq(lines.head, lines.last)
    else lines

  def tastySpan: Seq[Int] =
    span.map(_ - 1)

  def showSpan: String = span.mkString("(", ", ", ")")

  def filterTasty(f: Int => Boolean): SourceLines = copy(lines = lines.filter(l => f(l - 1)))

  def last: SourceLines = copy(lines = lines.lastOption.toSeq)

  def isEmpty: Boolean = lines.isEmpty

object SourceLines:
  def apply(sourceName: String, lines: Seq[Int]): SourceLines =
    new SourceLines(sourceName, lines.distinct.sorted)
