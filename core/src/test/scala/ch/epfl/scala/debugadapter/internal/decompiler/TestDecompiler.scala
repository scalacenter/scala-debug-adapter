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
                      |class Foo{
                      |  val value = "hello"
                      |
                      |  def hello: String = value
                      |}
                      |""".stripMargin

      val runner = MainDebuggeeRunner.mainClassRunner(
        source,
        "",
        ScalaVersion.`2.12`
      )

      val bytes = runner.projectEntry.readBytes("foo/Foo.class")

      println(bytes.size)

      val text = Decompiler.sourceNameAndText("Foo.class", "Foo", bytes)
      
      println(text)
    }

    // "should decompile my setter" - {

    //   val source = """|

    //                   |package foo
    //                   |
    //                   |class Foo2{
    //                   |   val value = "hello"
    //                   |}
    //                   |
    //                   |class Main2 extends App{
    //                   |
    //                   |   val foo2 = new Foo
    //                   |   foo2.value = "how are you"
    //                   |}
    //                   |
    //                   |""".stripMargin

    //   val runner = MainDebuggeeRunner.mainClassRunner(
    //     source,
    //     "foo.Main2",
    //     ScalaVersion.`2.12`
    //   )

    //   val bytes = runner.projectEntry.readBytes("foo/Main2.class")

    // }

  }

  // Mixin
//   val test3: Tests = Tests {

//     "should decompile my class" - {

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

//     }

//   }

//   // Bridge
//   val test4: Tests = Tests {

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
//                       |   val d = D.value
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

//     }

//   }
}
