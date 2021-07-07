class Foo {
  def bar(implicit x: String) = "barfoo" + x
}

object Bar {
  val foo = "foobar"
}

object EvaluateTest {
  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3)
    var b = 10
    implicit val c = "c"
    val x = new Foo()
    println(Bar.foo)
  }

  def inc(): String = {
    y += 10
    s"$y"
  }

  private var y = 10.12
  private val z = "20"
}