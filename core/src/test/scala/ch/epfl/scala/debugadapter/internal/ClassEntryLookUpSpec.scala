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
      val tempDir = sbt.io.IO.createTemporaryDirectory
      try {
        val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, ScalaVersion.`2.12`)
        val lookUp = ClassEntryLookUp(runner.projectEntry)

        val expectedSourceFile = runner.source.toUri
        val expectedClassName = "scaladebug.test.BreakpointTest$Hello$InnerHello$1"
        
        val className = lookUp.getFullyQualifiedClassName(expectedSourceFile, 14)
        assert(className.contains(expectedClassName))

        val sourceFile = lookUp.getSourceFile(expectedClassName)
        assert(sourceFile.contains(expectedSourceFile))
      } finally {
        sbt.io.IO.delete(tempDir)
      }
    }

    "should map source files to class names and backward, in dependency jars" - {
      val tempDir = sbt.io.IO.createTemporaryDirectory
      try {
        val classPathEntry = Coursier.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
        val lookUp = ClassEntryLookUp(classPathEntry)

        val sourceJar = classPathEntry.sourceEntries
          .collectFirst { case SourceJar(jar) => jar }
          .get
        val expectedSourceFile = URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")
        val expectedClassName = "cats.instances.ListInstances$$anon$1"
        
        val className = lookUp.getFullyQualifiedClassName(expectedSourceFile, 28)
        assert(className.contains(expectedClassName))

        val sourceFile = lookUp.getSourceFile(expectedClassName)
        assert(sourceFile.contains(expectedSourceFile))
      } finally {
        sbt.io.IO.delete(tempDir)
      }
    }

    "should get source file content, in project" - {
      val tempDir = sbt.io.IO.createTemporaryDirectory
      try {
        val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, ScalaVersion.`2.12`)
        val lookUp = ClassEntryLookUp(runner.projectEntry)
        
        val sourceFile = runner.source.toUri
        val expectedSourceContent = sbt.io.IO.read(runner.source.toFile)
        
        val sourceContent = lookUp.getSourceContent(sourceFile)
        assert(sourceContent.contains(expectedSourceContent))
      } finally {
        sbt.io.IO.delete(tempDir)
      }
    }

    "should get source file content, in dependency jar" - {
      val tempDir = sbt.io.IO.createTemporaryDirectory
      try {
        val classPathEntry = Coursier.fetchOnly("org.typelevel", "cats-core_2.12", "2.3.0")
        val lookUp = ClassEntryLookUp(classPathEntry)

        val sourceJar = classPathEntry.sourceEntries
          .collectFirst { case SourceJar(jar) => jar }
          .get
        val sourceFile = URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")
        
        val sourceFileContent = lookUp.getSourceContent(sourceFile)
        val contentSize = sourceFileContent.fold(0)(_.size)
        assert(contentSize == 8717)
      } finally {
        sbt.io.IO.delete(tempDir)
      }
    }
  }
}
