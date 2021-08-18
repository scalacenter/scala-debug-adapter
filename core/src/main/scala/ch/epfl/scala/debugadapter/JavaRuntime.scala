package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.internal.IO

import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.JavaConverters._

sealed trait JavaRuntime extends ClassEntry

final case class Java8(classJars: Seq[Path], sourceZip: Path) extends JavaRuntime {
  override def sourceEntries: Seq[SourceEntry] = Seq(SourceJar(sourceZip))
  override def classSystems: Seq[ClassSystem] = classJars.map(ClassJar.apply)
}

final case class Java9OrAbove(fsJar: Path, javaHome: Path, sourceZip: Path) extends JavaRuntime {
  override def sourceEntries: Seq[SourceEntry] = Seq(SourceJar(sourceZip))
  override def classSystems: Seq[ClassSystem] = {
    val classLoader = new URLClassLoader(Array(fsJar.toUri.toURL))
    Seq(JavaRuntimeSystem(classLoader, javaHome))
  }
}

object JavaRuntime {
  def apply(javaHome: String): Option[JavaRuntime] =
    JavaRuntime(Paths.get(javaHome))
  
  def apply(javaHome: Path): Option[JavaRuntime] = {
    for {
      srcZip <- Seq("src.zip", "lib/src.zip")
        .map(javaHome.resolve)
        .find(Files.exists(_))
      javaRuntime <- java8(javaHome, srcZip)
        .orElse(java9OrAbove(javaHome, srcZip))
    } yield javaRuntime
  }

  private def java8(javaHome: Path, srcZip: Path): Option[JavaRuntime] = {
    for {
      runtimeJar <- Seq("jre/lib/rt.jar", "lib/rt.jar")
        .map(javaHome.resolve)
        .find(Files.exists(_))
    } yield {
      val otherJars = Seq("jre/lib/charsets.jar", "lib/charsets.jar")
        .map(javaHome.resolve)
        .filter(Files.exists(_))
      Java8(Seq(runtimeJar) ++ otherJars, srcZip)
    }
  }

  private def java9OrAbove(javaHome: Path, srcZip: Path): Option[JavaRuntime] = {
    Some("lib/jrt-fs.jar")
      .map(javaHome.resolve)
      .filter(Files.exists(_))
      .map(Java9OrAbove(_, javaHome, srcZip))
  }
}
