package dap.internal

private[dap] final class Synchronized[A](var value: A) {
  def transform(f: A => A): Unit =
    synchronized {
      value = f(value)
    }
  
  def run[B](f: A => (A, B)): B = synchronized {
    synchronized {
      val (newValue, result) = f(value)
      value = newValue
      result
    }
  }
}
