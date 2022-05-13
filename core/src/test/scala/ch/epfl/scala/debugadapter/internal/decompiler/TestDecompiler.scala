package ch.epfl.scala.debugadapter.internal.decompiler

import utest._
import ch.epfl.scala.debugadapter.MainDebuggeeRunner
import ch.epfl.scala.debugadapter.ScalaVersion

object DecompilerSuite extends TestSuite {

  // Getters
  override val tests: Tests = Tests {

    "should decompile my class" - {

      val source = """|
                      |package foo
                      |
                      |trait A {
                      |  def foo: String = {
                      |   val x: Int = 0
                      |   return x.asInstanceOf[String]
                      | }
                      |}
                      |
                      |class B () extends A
                      |
                      |object Main2 {
                      |  def main(args: Array[String]): Unit = {
                      |    val b = new B
                      |    println(b.foo)
                      |  }
                      |}
                      |""".stripMargin

      val runner = MainDebuggeeRunner.mainClassRunner(
        source,
        "",
        ScalaVersion.`2.12`
      )

      val bytes = runner.projectEntry.readBytes("foo/B.class")

      println(bytes.size)
      val text =
        for (b <- bytes)
          yield (Decompiler.sourceNameAndText("A.class", "Foo", b))

      for (t <- text) print(t.get)
    }

//     "should decompile my setter" - {

//       val source = """|

//                       |package foo
//                       |
//                       |class Foo2{
//                       |   var value = "hello"
//                       |}
//                       |
//                       |class Main2 extends App{
//                       |
//                       |   val foo2 = new Foo2
//                       |   foo2.value = "how are you"
//                       |}
//                       |
//                       |""".stripMargin

//       val runner = MainDebuggeeRunner.mainClassRunner(
//         source,
//         "foo.Main2",
//         ScalaVersion.`2.12`
//       )

//       val bytes = runner.projectEntry.readBytes("foo/Main2.class")

//       val text = for(b <- bytes) yield(Decompiler.sourceNameAndText("Foo.class", "Foo", b))

//       for(t <- text) print(t)
//     }

//   // Mixin
//   "should decompile my mixin" - {

//       val source = """|

//                       |package foo
//                       |
//                       |trait A {
//                       |   def value: String = "Hi"
//                       |}
//                       |
//                       |class B() extends A
//                       |
//                       |class Main3 extends App{
//                       |
//                       |   val b = new B
//                       |   val a = b.value
//                       |
//                       |}
//                       |
//                       |""".stripMargin

//       val runner = MainDebuggeeRunner.mainClassRunner(
//         source,
//         "foo.Main3",
//         ScalaVersion.`2.12`
//       )

//       val bytes = runner.projectEntry.readBytes("foo/Main3.class")

//       val text = for(b <- bytes) yield(Decompiler.sourceNameAndText("Foo.class", "Foo", b))

//       for(t <- text) print(t)
//     }

// //   }

// //   // Bridge
//     "should decompile my class" - {

//       val source = """|

//                       |package foo
//                       |
//                       |class C {
//                       |   def value: String = "Hi"
//                       |}
//                       |
//                       |class D extends C {
//                       |  override def value: String = "Bonjour"
//                       |}
//                       |
//                       |class Main4 extends App{
//                       |
//                       |   val a = new D
//                       |   val d = a.value
//                       |
//                       |}
//                       |
//                       |""".stripMargin

//       val runner = MainDebuggeeRunner.mainClassRunner(
//         source,
//         "foo.Main4",
//         ScalaVersion.`2.12`
//       )

//       val bytes = runner.projectEntry.readBytes("foo/Main4.class")

//       val text = for(b <- bytes) yield(Decompiler.sourceNameAndText("Foo.class", "Foo", b))

//       for(t <- text) print(t)
//     }
  }
}
