package ch.epfl.scala.debugadapter

import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

sealed trait JavaRuntime extends ClassEntry {
  def javaHome: Path
  def name: String = javaHome.getFileName.toString
}

final case class Java8(javaHome: Path, classJars: Seq[Path], sourceZip: Option[Path]) extends JavaRuntime {
  override def sourceEntries: Seq[SourceEntry] = sourceZip.map(SourceJar.apply).toSeq
  override def classSystems: Seq[ClassSystem] = classJars.map(ClassJar.apply)
}

final case class Java9OrAbove(javaHome: Path, fsJar: Path, sourceZip: Option[Path]) extends JavaRuntime {
  override def sourceEntries: Seq[SourceEntry] = sourceZip.map(SourceJar.apply).toSeq
  override def classSystems: Seq[JavaRuntimeSystem] = {
    val classLoader = new URLClassLoader(Array(fsJar.toUri.toURL))
    Seq(JavaRuntimeSystem(classLoader, javaHome))
  }
}

object JavaRuntime {
  def apply(javaHome: String): Option[JavaRuntime] =
    JavaRuntime(Paths.get(javaHome))

  def apply(javaHome: Path): Option[JavaRuntime] = {
    val sources = Seq("src.zip", "lib/src.zip", "../src.zip")
      .map(javaHome.resolve)
      .find(Files.exists(_))
    java8(javaHome, sources).orElse(java9OrAbove(javaHome, sources))
  }

  def applyWithoutSources(javaHome: String): Option[JavaRuntime] = {
    java8(Paths.get(javaHome), None).orElse(java9OrAbove(Paths.get(javaHome), None))
  }

  private def java8(javaHome: Path, srcZip: Option[Path]): Option[JavaRuntime] = {
    for {
      runtimeJar <- Seq("jre/lib/rt.jar", "lib/rt.jar")
        .map(javaHome.resolve)
        .find(Files.exists(_))
    } yield {
      val otherJars = Seq("jre/lib/charsets.jar", "lib/charsets.jar")
        .map(javaHome.resolve)
        .filter(Files.exists(_))
      Java8(javaHome, Seq(runtimeJar) ++ otherJars, srcZip)
    }
  }

  private def java9OrAbove(
      javaHome: Path,
      srcZip: Option[Path]
  ): Option[JavaRuntime] = {
    Some("lib/jrt-fs.jar")
      .map(javaHome.resolve)
      .filter(Files.exists(_))
      .map(Java9OrAbove(javaHome, _, srcZip))
  }
}
