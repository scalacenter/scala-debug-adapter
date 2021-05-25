class Foo {
  def bar() = "foobar"
}

object Bar {
  def foo(implicit x: String) = "barfoo" + x
}

object EvaluateTest {
  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3)
    val b = 10
    implicit val c = "c"
    val x = new Foo()
    println(x.bar())
  }

  def inc(): String = {
    y += 10
    s"$y"
  }

  private var y = 10
}