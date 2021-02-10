package ch.epfl.scala.debugadapter.internal

private[debugadapter] final class Synchronized[A](var value: A) {
  def transform(f: A => A): Unit =
    synchronized {
      value = f(value)
    }
}
