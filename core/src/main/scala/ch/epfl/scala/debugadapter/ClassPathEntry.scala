package ch.epfl.scala.debugadapter

import java.nio.file.Path
import java.nio.file.Files
import java.nio.file.Paths

case class ClassPathEntry(absolutePath: Path, sourceEntries: Seq[SourceEntry]) {
  def name: String = absolutePath.getFileName.toString.stripSuffix(".jar")
  def isJar: Boolean = absolutePath.toString.endsWith(".jar")
}

object ClassPathEntry {
  def javaRuntime(javaHome: String): Option[ClassPathEntry] =
    javaRuntime(Paths.get(javaHome))

  /**
    * From Java 9 onward this method returns null
    * That's because the rt.jar does not exist anymore
    * See https://openjdk.java.net/jeps/220 for more information
    * 
    * As a consequence it is not yet possible to step into jdk classes
    * in Java 9 or later.
    */
  def javaRuntime(javaHome: Path): Option[ClassPathEntry] = {
    for {
      runtimeJar <- Seq("jre/lib/rt.jar", "lib/rt.jar")
        .map(javaHome.resolve)
        .find(Files.exists(_))
      srcZip <- Seq("src.zip", "lib/src.zip")
        .map(javaHome.resolve)
        .find(Files.exists(_))
    } yield ClassPathEntry(runtimeJar, Seq(SourceJar(srcZip)))
  }
}
