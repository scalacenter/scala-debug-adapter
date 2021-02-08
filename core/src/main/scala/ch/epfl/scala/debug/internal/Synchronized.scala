package ch.epfl.scala.debug.internal

private[debug] final class Synchronized[A](var value: A) {
  def transform(f: A => A): Unit =
    synchronized {
      value = f(value)
    }
}
