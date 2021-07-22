package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Coursier
import ch.epfl.scala.debugadapter.MainDebuggeeRunner
import ch.epfl.scala.debugadapter.ScalaVersion
import ch.epfl.scala.debugadapter.SourceJar
import sbt.io.IO
import utest._

import java.net.URI

object ClassPathEntryLookUpSpec extends TestSuite {
  def tests = Tests {
    "should map source files to class names and backward, in project" - {
      val tempDir = IO.createTemporaryDirectory
      try {
        val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, ScalaVersion.`2.12`)
        val classPathEntry = runner.projectEntry
        val lookUp = ClassPathEntryLookUp(classPathEntry)

        val expectedSourceFile = runner.source.toUri
        val expectedClassName = "scaladebug.test.BreakpointTest$Hello$InnerHello$1"
        
        val className = lookUp.getFullyQualifiedClassName(expectedSourceFile, 14)
        assert(className.contains(expectedClassName))

        val sourceFile = lookUp.getSourceFile(expectedClassName)
        assert(sourceFile.contains(expectedSourceFile))
      } finally {
        IO.delete(tempDir)
      }
    }

    "should map source files to class names and backward, in dependency jars" - {
      val tempDir = IO.createTemporaryDirectory
      try {
        val classPathEntry = Coursier.fetchOnly("org.typelevel", "cats-core_3", "2.6.1")
        val lookUp = ClassPathEntryLookUp(classPathEntry)

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
        IO.delete(tempDir)
      }
    }

    "should get source file content, in project" - {
      val tempDir = IO.createTemporaryDirectory
      try {
        val runner = MainDebuggeeRunner.scalaBreakpointTest(tempDir, ScalaVersion.`2.12`)
        val classPathEntry = runner.projectEntry
        val lookUp = ClassPathEntryLookUp(classPathEntry)
        
        val sourceFile = runner.source.toUri
        val expectedSourceContent = IO.read(runner.source.toFile)
        
        val sourceContent = lookUp.getSourceContent(sourceFile)
        assert(sourceContent.contains(expectedSourceContent))
      } finally {
        IO.delete(tempDir)
      }
    }

    "should get source file content, in dependency jar" - {
      val tempDir = IO.createTemporaryDirectory
      try {
        val classPathEntry = Coursier.fetchOnly("org.typelevel", "cats-core_2.12", "2.3.0")
        val lookUp = ClassPathEntryLookUp(classPathEntry)

        val sourceJar = classPathEntry.sourceEntries
          .collectFirst { case SourceJar(jar) => jar }
          .get
        val sourceFile = URI.create(s"jar:${sourceJar.toUri}!/cats/instances/list.scala")
        
        val sourceFileContent = lookUp.getSourceContent(sourceFile)
        val contentSize = sourceFileContent.fold(0)(_.size)
        assert(contentSize == 8717)
      } finally {
        IO.delete(tempDir)
      }
    }
  }
}
