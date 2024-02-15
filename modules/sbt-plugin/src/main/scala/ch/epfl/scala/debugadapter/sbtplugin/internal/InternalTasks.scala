package ch.epfl.scala.debugadapter.sbtplugin.internal

import ch.epfl.scala.debugadapter._
import sbt.{ScalaVersion => _, _}

import scala.util.Properties
import _root_.io.reactivex.Observable
import ch.epfl.scala.debugadapter.sbtplugin.DebugAdapterPlugin.autoImport._

private[sbtplugin] object InternalTasks {
  lazy val modules: Def.Initialize[Task[Seq[Module]]] = Def.taskDyn {
    val _ = Keys.fullClasspath.value // compile to fill the class directories
    val modules = for {
      (proj, configs) <- Keys.bspInternalDependencyConfigurations.value
      config <- configs
    } yield module(proj, config)
    modules.join(_.join)
  }

  lazy val allClassUpdates: Def.Initialize[Observable[Seq[String]]] = Def.settingDyn {
    val internalDependencies = Keys.bspInternalDependencyConfigurations
    val observables = for {
      (proj, configs) <- Keys.bspInternalDependencyConfigurations.value
      config <- configs
    } yield (proj / config / debugAdapterClassUpdates)
    Def.setting {
      observables.join.value.fold(Observable.empty[Seq[String]])(_ mergeWith _)
    }
  }

  lazy val libraries: Def.Initialize[Task[Seq[Library]]] = Def.task {
    val classifierReport = Keys.updateClassifiers.value
    val report = Keys.update.value
    val configRef = Keys.configuration.value.toConfigRef
    val allSourceJars = classifierReport.configurations
      .filter(report => report.configuration == configRef)
      .flatMap(_.modules)
      .map { module =>
        val sourceJars = module.artifacts.collect {
          case (artifact, jar) if artifact.classifier.contains("sources") =>
            SourceJar(jar.toPath)
        }
        (module.module, sourceJars)
      }
      .toMap

    report.configurations
      .filter(report => report.configuration == configRef)
      .flatMap(_.modules)
      .flatMap { module =>
        val sourceEntries = allSourceJars.getOrElse(module.module, Seq.empty)
        module.artifacts.collectFirst {
          case (artifact, jar) if !artifact.classifier.exists(cls => cls == "sources" || cls == "javadoc") =>
            Library(artifact.name, module.module.revision, jar.toPath, sourceEntries)
        }
      }
  }

  lazy val unmanagedEntries: Def.Initialize[Task[Seq[UnmanagedEntry]]] = Def.task {
    val fullClasspath = Keys.fullClasspath.value
    val managedEntries = libraries.value ++ modules.value
    val managedClasspath = managedEntries.map(_.absolutePath).toSet
    fullClasspath
      .map(_.data.getAbsoluteFile.toPath)
      .filter(path => !managedClasspath.contains(path))
      .map(UnmanagedEntry.apply)
  }

  lazy val javaRuntime: Def.Initialize[Task[Option[JavaRuntime]]] = Def.task {
    for {
      jdkHome <- Keys.javaHome.value
        .map(_.toString)
        .orElse(Option(Properties.jdkHome))
      javaRuntime <- JavaRuntime(jdkHome)
    } yield javaRuntime
  }

  lazy val debugToolsResolver: Def.Initialize[Task[xsbti.Logger => DebugToolsResolver]] = Def.task {
    val dependencyRes = Keys.dependencyResolution.value
    val updateConfig = Keys.updateConfiguration.value
    val warningConfig = (Keys.update / Keys.unresolvedWarningConfiguration).value
    val scalaInstance = Keys.scalaInstance.value
    logger => new SbtDebugToolsResolver(scalaInstance, dependencyRes, updateConfig, warningConfig, logger)
  }

  private def module(proj: ProjectRef, config: ConfigKey): Def.Initialize[Task[Module]] = Def.task {
    val classDirectory = (proj / config / Keys.classDirectory).value.toPath
    val sourceDirectories = (proj / config / Keys.sourceDirectories).value.map(_.toPath)
    val sourceFiles = (proj / config / Keys.sources).value.map(_.toPath)
    val standaloneSourceFiles = sourceFiles.filter { file =>
      sourceDirectories.forall(dir => !file.startsWith(dir))
    }
    val name = (proj / config / Keys.bspTargetIdentifier).value.uri.toString
    val scalaVersion = (proj / config / Keys.scalaVersion).?.value.map(ScalaVersion.apply)
    val scalacOptions = (proj / config / Keys.scalacOptions).?.value.toSeq.flatten
    val sourceEntries =
      sourceDirectories.map(SourceDirectory.apply) ++
        standaloneSourceFiles.map(f => StandaloneSourceFile(f, f.getFileName.toString))
    Module(name, scalaVersion, scalacOptions, classDirectory, sourceEntries)
  }
}
