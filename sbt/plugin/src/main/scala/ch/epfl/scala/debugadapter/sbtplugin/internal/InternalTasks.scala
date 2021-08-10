package ch.epfl.scala.debugadapter.sbtplugin.internal

import sbt._
import ch.epfl.scala.debugadapter._

private[sbtplugin] object InternalTasks {
  def classPathEntries: Def.Initialize[Task[Seq[ClassPathEntry]]] = Def.task {
    val _ = Keys.compile.value // compile to fill the class directories
    externalClassPathEntries.value ++ internalClassPathEntries.value
  }

  private def externalClassPathEntries: Def.Initialize[Task[Seq[ClassPathEntry]]] = Def.task {
    val classifierReport = Keys.updateClassifiers.value
    val report = Keys.update.value
    val configRef = Keys.configuration.value.toConfigRef
    val allSourceJars = classifierReport
      .configurations
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
    
    report
      .configurations
      .filter(report => report.configuration == configRef)
      .flatMap(_.modules)
      .flatMap { module =>
        val sourceEntries = allSourceJars.getOrElse(module.module, Seq.empty)
        module.artifacts.collectFirst {
          case (artifact, jar) if artifact.classifier.isEmpty => 
            ClassPathEntry(jar.toPath, sourceEntries) 
        }
      }
  }

  private def internalClassPathEntries: Def.Initialize[Task[Seq[ClassPathEntry]]] = Def.taskDyn {
    val internalDependencies = Keys.bspInternalDependencyConfigurations
    val classPathEntries = for {
      (proj, configs) <- Keys.bspInternalDependencyConfigurations.value
      config <- configs
    } yield internalClassPathEntry(proj, config)
    classPathEntries.join(_.join)
  }

  private def internalClassPathEntry(proj: ProjectRef, config: ConfigKey): Def.Initialize[Task[ClassPathEntry]] = Def.task {
    val classDirectory = (proj / config / Keys.classDirectory).value.toPath
    val sourceDirectories = (proj / config / Keys.sourceDirectories).value.map(_.toPath)
    val sourceFiles = (proj / config / Keys.sources).value.map(_.toPath)
    val standaloneSourceFiles = sourceFiles.filter { file => 
      sourceDirectories.forall(dir => !file.startsWith(dir))
    }
    val sourceEntries =
      sourceDirectories.map(SourceDirectory.apply) ++
        standaloneSourceFiles.map(f => StandaloneSourceFile(f, f.getFileName.toString))
    ClassPathEntry(classDirectory, sourceEntries)
  }
}