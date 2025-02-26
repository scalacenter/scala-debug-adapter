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
      lookUp.getFullyQualifiedClassName(SanitizedUri(expectedSourceFile), 14)
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
      lookUp.getFullyQualifiedClassName(SanitizedUri(expectedSourceFile), 28)
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

  test("case-sensitivity of the OS") {
    val debuggee = TestingDebuggee.scalaBreakpointTest(ScalaVersion.`2.12`)
    val lookUp = ClassEntryLookUp(debuggee.mainModule, NoopLogger)

    val sourceFile = debuggee.sourceFiles.head.toString
    assert(sourceFile.contains("scala-debug-adapter"))

    val moddedSourceFile = sourceFile.replace("scala-debug-adapter", "SCALA-deBug-Adapter")
    val sourceUri = SanitizedUri(toUri(moddedSourceFile))
    val className = lookUp.getFullyQualifiedClassName(sourceUri, 14)

    val expectedClassName = if (isFilesystemCaseInsensitive) Some("example.Main$Hello$InnerHello$1") else None
    assertEquals(className, expectedClassName)
  }

  test("case-sensitivity in jar path") {
    val catsCore = TestingResolver.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
    val lookUp = ClassEntryLookUp(catsCore, NoopLogger)

    val sourceJar = catsCore.sourceEntries.collectFirst { case SourceJar(jar) => jar }.get.toString
    assert(sourceJar.contains("cats-core")) // precondition for this test

    val moddedSourceJar = sourceJar.replace("cats-core", "Cats-CoRe")
    val moddedsourceFile = URI.create(s"jar:${toUri(moddedSourceJar)}!/cats/instances/list.scala")
    val expectedClassName = if (isFilesystemCaseInsensitive) Some("cats.instances.ListInstances$$anon$1") else None
    val obtainedClassName = lookUp.getFullyQualifiedClassName(SanitizedUri(moddedsourceFile), 28)
    assertEquals(obtainedClassName, expectedClassName)

    val invalidSourceFile = URI.create(s"jar:file:${toUri(sourceJar)}!/caTs/Instances/list.scala")
    val notFound = lookUp.getFullyQualifiedClassName(SanitizedUri(invalidSourceFile), 28)
    assert(notFound.isEmpty)
  }

  private def toUri(path: String): URI = Paths.get(path).toUri
}
