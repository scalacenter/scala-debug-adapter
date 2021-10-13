package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Coursier
import ch.epfl.scala.debugadapter.MainDebuggeeRunner
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.SourceJar
import utest._

import java.net.URI
import java.nio.file.Files

object ClassEntryLookUpSpec extends TestSuite {
  def tests = Tests {
    "should map source files to class names and backward, in project" - {
      val runner =
        MainDebuggeeRunner.scalaBreakpointTest(ScalaVersion.`2.12`)
      val lookUp = ClassEntryLookUp(runner.projectEntry)

      val expectedSourceFile = runner.source.toUri
      val expectedClassName =
        "scaladebug.test.BreakpointTest$Hello$InnerHello$1"

      val className =
        lookUp.getFullyQualifiedClassName(expectedSourceFile, 14)
      assert(className.contains(expectedClassName))

      val sourceFile = lookUp.getSourceFile(expectedClassName)
      assert(sourceFile.contains(expectedSourceFile))
    }

    "should map source files to class names and backward, in dependency jars" - {
      val classPathEntry =
        Coursier.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
      val lookUp = ClassEntryLookUp(classPathEntry)

      val sourceJar = classPathEntry.sourceEntries.collectFirst {
        case SourceJar(jar) => jar
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
      val runner =
        MainDebuggeeRunner.scalaBreakpointTest(ScalaVersion.`2.12`)
      val lookUp = ClassEntryLookUp(runner.projectEntry)

      val sourceFile = runner.source.toUri
      val expectedSourceContent = new String(Files.readAllBytes(runner.source))

      val sourceContent = lookUp.getSourceContent(sourceFile)
      assert(sourceContent.contains(expectedSourceContent))
    }

    "should get source file content, in dependency jar" - {
      val classPathEntry =
        Coursier.fetchOnly("org.typelevel", "cats-core_2.12", "2.3.0")
      val lookUp = ClassEntryLookUp(classPathEntry)

      val sourceJar = classPathEntry.sourceEntries.collectFirst {
        case SourceJar(jar) => jar
      }.get
      val sourceFile =
        URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")

      val sourceFileContent = lookUp.getSourceContent(sourceFile)
      val contentSize = sourceFileContent.fold(0)(_.size)
      assert(contentSize == 8717)
    }
  }
}
