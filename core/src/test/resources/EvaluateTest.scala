object EvaluateTest {
  val a = 1
  val b = 2

  def main(args: Array[String]): Unit = {
    val c = Num(9)
    val d = Num(5)
    val e = 3
    val f = 4
    println(a + b)
    println(c.add(d))
    println(add(e, f))
  }

  def add(x: Int, y: Int): Int = x + y

  def add(x: Double, y: Double): Double = x + y

  def add(x: Num, y: Num): Int = x.add(y).value

  case class Num(value: Int) {
    def add(num: Num): Num = Num(value + num.value)
  }
}