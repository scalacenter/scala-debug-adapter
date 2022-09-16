package ch.epfl.scala.debugadapter

import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

sealed trait JavaRuntime extends ClassEntry

final case class Java8(javaHome: Path, classJars: Seq[Path], sourceZip: Path)
    extends JavaRuntime {
  override def sourceEntries: Seq[SourceEntry] = Seq(SourceJar(sourceZip))
  override def classSystems: Seq[ClassSystem] = classJars.map(ClassJar.apply)
  override def name: String = javaHome.toString
}

final case class Java9OrAbove(javaHome: Path, fsJar: Path, sourceZip: Path)
    extends JavaRuntime {
  override def sourceEntries: Seq[SourceEntry] = Seq(SourceJar(sourceZip))
  override def classSystems: Seq[JavaRuntimeSystem] = {
    val classLoader = new URLClassLoader(Array(fsJar.toUri.toURL))
    Seq(JavaRuntimeSystem(classLoader, javaHome))
  }
  override def name: String = javaHome.toString
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
      Java8(javaHome, Seq(runtimeJar) ++ otherJars, srcZip)
    }
  }

  private def java9OrAbove(
      javaHome: Path,
      srcZip: Path
  ): Option[JavaRuntime] = {
    Some("lib/jrt-fs.jar")
      .map(javaHome.resolve)
      .filter(Files.exists(_))
      .map(Java9OrAbove(javaHome, _, srcZip))
  }
}
