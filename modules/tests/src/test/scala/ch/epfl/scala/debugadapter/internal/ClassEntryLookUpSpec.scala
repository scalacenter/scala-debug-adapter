package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.testfmk.TestingResolver
import ch.epfl.scala.debugadapter.testfmk.TestingDebuggee
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.SourceJar

import java.net.URI
import ch.epfl.scala.debugadapter.testfmk.NoopLogger
import munit.FunSuite
import scala.util.Properties
import java.nio.file.Paths

class ClassEntryLookUpSpec extends FunSuite {
  val isFilesystemCaseInsensitive = Properties.isWin || Properties.isMac

  test("should map source files to class names and backward, in project") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(ScalaVersion.`2.12`)
    val lookUp = ClassEntryLookUp(debuggee.mainModule, NoopLogger)

    val expectedSourceFile = debuggee.sourceFiles.head.toUri
    val expectedClassName =
      "example.Main$Hello$InnerHello$1"

    val className =
      lookUp.getFullyQualifiedClassName(SourceFileKey(expectedSourceFile), 14)
    assert(className.contains(expectedClassName))

    val sourceFile = lookUp.getSourceFileURI(expectedClassName)
    assert(sourceFile.contains(expectedSourceFile))
  }

  test("should map source files to class names and backward, in dependency jars") {
    val catsCore = TestingResolver.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
    val lookUp = ClassEntryLookUp(catsCore, NoopLogger)

    val sourceJar = catsCore.sourceEntries.collectFirst { case SourceJar(jar) =>
      jar
    }.get
    val expectedSourceFile = URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")

    val expectedClassName = "cats.instances.ListInstances$$anon$1"

    val className =
      lookUp.getFullyQualifiedClassName(SourceFileKey(expectedSourceFile), 28)
    assert(className.contains(expectedClassName))

    val sourceFile = lookUp.getSourceFileURI(expectedClassName)
    assert(sourceFile.contains(expectedSourceFile))
  }

  test("should get source file content, in project") {
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

  test("should get source file content, in dependency jar") {
    val catsCore = TestingResolver.fetchOnly("org.typelevel", "cats-core_2.12", "2.3.0")
    val lookUp = ClassEntryLookUp(catsCore, NoopLogger)

    val sourceJar = catsCore.sourceEntries.collectFirst { case SourceJar(jar) =>
      jar
    }.get
    val sourceFile = URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")

    val sourceFileContent = lookUp.getSourceContent(sourceFile)
    val contentSize = sourceFileContent.fold(0)(_.size)
    assert(contentSize == 8717)
  }

  test("should work in case of broken dependency jar") {
    val swaggerUi = TestingResolver.fetchOnly("org.webjars", "swagger-ui", "4.2.1")
    val lookUp = ClassEntryLookUp(swaggerUi, NoopLogger)
    val sourceJar = swaggerUi.sourceEntries.collectFirst { case SourceJar(jar) =>
      jar
    }
    assert(sourceJar.isDefined)
    assert(lookUp.sources.isEmpty)
  }

  test("project class lookups should accomodate case-sensitivity of the OS filesystem") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(ScalaVersion.`2.12`)
    val lookUp = ClassEntryLookUp(debuggee.mainModule, NoopLogger)

    val sourceFilePath = debuggee.sourceFiles.head;

    // Change case of file.
    val sourceFilePathString = sourceFilePath.toString()
    assert(sourceFilePathString.contains("scala-debug-adapter")) // precondition for this test.

    // Change some casing of the path uri
    val lookupKey =
      SourceFileKey(Paths.get(sourceFilePathString.replace("scala-debug-adapter", "SCALA-deBug-Adapter")).toUri)

    val expectedClassName = "example.Main$Hello$InnerHello$1"

    // Note: The expected result is different based on the OS this test is running on.
    val className = lookUp.getFullyQualifiedClassName(lookupKey, 14)
    assertEquals(isFilesystemCaseInsensitive, className.contains(expectedClassName))
  }

  test("dependency jar source lookups should accomodate case-sensitivity of the OS filesystem") {
    val catsCore = TestingResolver.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
    val lookUp = ClassEntryLookUp(catsCore, NoopLogger)

    val sourceJar = catsCore.sourceEntries.collectFirst { case SourceJar(jar) =>
      jar
    }.get

    val sourceJarPathString = sourceJar.toString()
    assert(sourceJarPathString.contains("cats-core")) // precondition for this test

    val expectedClassName = "cats.instances.ListInstances$$anon$1"

    {
      val moddedSourceJarPath = Paths.get(sourceJarPathString.replace("cats-core", "Cats-CoRe"));
      val moddedSourceJarUri = URI.create(s"jar:${moddedSourceJarPath.toUri()}!/cats/instances/list.scala")

      // Note: The expected results are different based on the OS this test is running on.
      val className = lookUp.getFullyQualifiedClassName(SourceFileKey(moddedSourceJarUri), 28)
      assertEquals(isFilesystemCaseInsensitive, className.contains(expectedClassName))
    }

    {
      val moddedSourceJarUri = URI.create(s"jar:${sourceJar.toUri()}!/caTs/Instances/list.scala")

      // The contents of Jar file is case-sensitive regardless of OS filesystem
      val className = lookUp.getFullyQualifiedClassName(SourceFileKey(moddedSourceJarUri), 28)
      assert(className.isEmpty)
    }

  }

}
