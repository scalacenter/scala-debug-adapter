package scala.tools.nsc

import utest._

import java.net.{URL, URLClassLoader}

object ExpressionCompilerTest extends TestSuite {
  def tests: Tests = Tests {
    "compile" - {
      val expressionCompiler = ExpressionCompiler(System.getProperty("java.class.path"), 5)
      val source =
        """object EvaluateTest {
          |  def main(args: Array[String]): Unit = {
          |    val a: String = "a"
          |    val b: String = "b"
          |    println("hello")
          |  }
          |}
          |""".stripMargin

      // it reports an error but works anyway
      expressionCompiler.compile(source, "a + b")

      val url = new URL("file://" + expressionCompiler.dir + "/")
      val urlClassLoader = new URLClassLoader(Array(url))
      val expressionClass = urlClassLoader.loadClass("Expression")
      val expression = expressionClass.newInstance()
      val method = expressionClass.getMethods.find(_.getName == "evaluate").get
      val result = method.invoke(expression, Array[Any]("a", "b"), Array[Any]("a", "b"))
      println("result = " + result)
    }
  }
}
