package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.SourceJar
import utest.*

import java.net.URI
import ch.epfl.scala.debugadapter.NoopLogger

object ClassEntryLookUpSpec extends TestSuite {
  def tests = Tests {
    "should map source files to class names and backward, in project" - {
      val debuggee = TestingDebuggee.scalaBreakpointTest(ScalaVersion.`2.12`)
      val lookUp = ClassEntryLookUp(debuggee.mainModule, NoopLogger)

      val expectedSourceFile = debuggee.sourceFiles.head.toUri
      val expectedClassName =
        "example.Main$Hello$InnerHello$1"

      val className =
        lookUp.getFullyQualifiedClassName(expectedSourceFile, 14)
      assert(className.contains(expectedClassName))

      val sourceFile = lookUp.getSourceFile(expectedClassName)
      assert(sourceFile.contains(expectedSourceFile))
    }

    "should map source files to class names and backward, in dependency jars" - {
      val catsCore = TestingResolver.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
      val lookUp = ClassEntryLookUp(catsCore, NoopLogger)

      val sourceJar = catsCore.sourceEntries.collectFirst { case SourceJar(jar) =>
        jar
      }.get
      val expectedSourceFile =
        URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")
      val expectedClassName = "cats.instances.ListInstances$$anon$1"

      val className =
        lookUp.getFullyQualifiedClassName(expectedSourceFile, 28)
      assert(className.contains(expectedClassName))

      val sourceFile = lookUp.getSourceFile(expectedClassName)
      assert(sourceFile.contains(expectedSourceFile))
    }

    "should get source file content, in project" - {
      val source =
        """|package example
           |
           |object Main {
           |  def main(args: Array[String]): Unit = {
           |    println("Hello, World!")
           |  }
           |}
           |""".stripMargin
      val mainClass = "example.Main"
      val debuggee = TestingDebuggee.mainClass(source, mainClass, ScalaVersion.`2.12`)
      val lookUp = ClassEntryLookUp(debuggee.mainModule, NoopLogger)

      val sourceContent = lookUp.getSourceContentFromClassName(mainClass)
      assert(sourceContent.contains(source))
    }

    "should get source file content, in dependency jar" - {
      val catsCore = TestingResolver.fetchOnly("org.typelevel", "cats-core_2.12", "2.3.0")
      val lookUp = ClassEntryLookUp(catsCore, NoopLogger)

      val sourceJar = catsCore.sourceEntries.collectFirst { case SourceJar(jar) =>
        jar
      }.get
      val sourceFile =
        URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")

      val sourceFileContent = lookUp.getSourceContent(sourceFile)
      val contentSize = sourceFileContent.fold(0)(_.size)
      assert(contentSize == 8717)
    }

    "should work in case of broken dependency jar" - {
      val swaggerUi = TestingResolver.fetchOnly("org.webjars", "swagger-ui", "4.2.1")
      val lookUp = ClassEntryLookUp(swaggerUi, NoopLogger)
      val sourceJar = swaggerUi.sourceEntries.collectFirst { case SourceJar(jar) =>
        jar
      }
      assert(sourceJar.isDefined)
      assert(lookUp.sources.isEmpty)
    }
  }
}
