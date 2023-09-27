package ch.epfl.scala.debugadapter.internal.binary

// SourceLine is 1-based, contrary to tastyquery.SourcePosition
opaque type SourceLine = Int

object SourceLine:
  inline def apply(line: Int): SourceLine = line

  given Ordering[SourceLine] with
    def compare(x: SourceLine, y: SourceLine): Int =
      java.lang.Integer.compare(x, y)

  extension (line: SourceLine) def toTasty: Int = line - 1
