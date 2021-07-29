class Foo {
  val x1 = 1
  val x2 = 2

  private val y1 = 3
  private var y2 = "foo"

  def a() = "a"

  def b(arg: Int): Int = arg

  def c(implicit arg: String): String = arg

  private def d() = "d"

  class InnerClass {
    val x1 = "foo"
    val z1 = 6

    private val y1 = 7
    private var y2 = 8

    def a(): String = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d(): String = "d"
  }

  private class PrivateInnerClass {
    val x1 = 9
    val z1 = 10

    private val y1 = 11
    private var y2 = 12

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }

  object InnerObject {
    val z1 = 13
    val z2 = 14

    private val y1 = 15
    private var y2 = 16

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }

  private object PrivateInnerObject {
    val x1 = 17
    val z1 = 18

    private val y1 = 19
    private var y2 = 20

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }
}

object Bar {
  val x1 = 21
  val x2 = 22

  private val y1 = 23
  private var y2 = 24

  def a() = "a"

  def b(arg: Int): Int = arg

  def c(implicit arg: String): String = arg

  private def d() = "d"

  class InnerClass {
    val x1 = 25
    val z1 = 26

    private val y1 = 27
    private var y2 = 28

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }

  private class PrivateInnerClass {
    val x1 = 29
    val z1 = 30

    private val y1 = 31
    private var y2 = 32

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }

  object InnerObject {
    val x1 = 33
    val z1 = 34

    private val y1 = 35
    private var y2 = 36

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }

  private object PrivateInnerObject {
    val x1 = 37
    val z1 = 38

    private val y1 = 39
    private var y2 = 40

    def a() = "a"

    def b(arg: Int): Int = arg

    def c(implicit arg: String): String = arg

    private def d() = "d"
  }
}

object EvaluateTest {
  def main(args: Array[String]): Unit = {
    val a = 1
    var b = 2.3
    implicit val c = "c"
    val foo = new Foo()

    foo.a()
    foo.b(1)
    foo.c

    Bar.a()
    Bar.b(1)
    Bar.c

    val fooInner = new foo.InnerClass()
    fooInner.a()
    fooInner.b(1)
    fooInner.c
  }
}