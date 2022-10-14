import sbt.CrossVersion
import sbt.Keys.crossVersion
import scala.collection.mutable

def isRelease() =
  System.getenv("GITHUB_REPOSITORY") == "scalacenter/scala-debug-adapter" &&
    System.getenv("GITHUB_WORKFLOW") == "Release"

def isCI = System.getenv("CI") != null

inThisBuild(
  Seq(
    organization := "ch.epfl.scala",
    homepage := Some(url("https://github.com/scalacenter/scala-debug-adapter")),
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := Developers.list,
    scalaVersion := Dependencies.scala212,
    version ~= { dynVer =>
      if (isRelease) dynVer
      else "3.0.2-SNAPSHOT" // only for local publishing
    },
    resolvers += Resolver.mavenLocal
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(
    // format: off
    core, tests, sbtPlugin, 
    expressionCompiler212, expressionCompiler213, expressionCompiler3,
    scala3StepFilter
    // format: on
  )
  .settings(
    publish / skip := true
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(SbtJdiTools, BuildInfoPlugin)
  .settings(
    name := "scala-debug-adapter",
    scalacOptions ++= Seq("-Xsource:3", "-Ywarn-unused-import"),
    libraryDependencies ++= List(
      Dependencies.scalaReflect,
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug,
      Dependencies.sbtTestAgent
    ),
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.action("organization")(organization.value),
      BuildInfoKey.action("version")(version.value),
      BuildInfoKey.action("expressionCompilerName")((expressionCompiler212 / name).value),
      BuildInfoKey.action("scala3StepFilterName")((scala3StepFilter / name).value),
      BuildInfoKey.action("defaultScala2Version")(Dependencies.scala213),
      BuildInfoKey.action("defaultScala3Version")(Dependencies.scala3)
    ),
    buildInfoPackage := "ch.epfl.scala.debugadapter"
  )

lazy val tests = project
  .in(file("tests"))
  .settings(
    name := "scala-debug-adapter-test",
    libraryDependencies ++= List(Dependencies.munit, Dependencies.coursier, Dependencies.coursierJvm),
    scalacOptions ++= Seq("-Xsource:3", "-Ywarn-unused-import"),
    PgpKeys.publishSigned := {},
    publish := {},
    // Test / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044",
    Test / fork := true,
    // do not use sbt logger, otherwise the output of a test only appears at the end of the suite
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "+l"),
    Test / test := (Test / test)
      .dependsOn(
        expressionCompiler212 / publishLocal,
        expressionCompiler213 / publishLocal,
        expressionCompiler3 / publishLocal,
        scala3StepFilter / publishLocal
      )
      .value
  )
  .dependsOn(core)

lazy val sbtPlugin = project
  .in(file("sbt-plugin"))
  .enablePlugins(SbtPlugin, ContrabandPlugin, JsonCodecPlugin)
  .settings(
    name := "sbt-debug-adapter",
    sbtVersion := "1.4.9",
    scriptedSbt := "1.5.5",
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false,
    scriptedDependencies := scriptedDependencies
      .dependsOn(publishLocal, core / publishLocal, tests / publishLocal)
      .value
  )
  .dependsOn(core)

lazy val expressionCompiler212 = expressionCompiler.jvm(Dependencies.scala212)
lazy val expressionCompiler213 = expressionCompiler.jvm(Dependencies.scala213)
lazy val expressionCompiler3 = expressionCompiler.jvm(Dependencies.scala3)
lazy val expressionCompiler = projectMatrix
  .in(file("expression-compiler"))
  .jvmPlatform(scalaVersions = Seq(Dependencies.scala3, Dependencies.scala213, Dependencies.scala212))
  .settings(
    name := "scala-expression-compiler",
    crossScalaVersions ++= {
      CrossVersion
        .partialVersion(scalaVersion.value)
        .collect {
          case (2, 12) => Seq("2.12.17", "2.12.16", "2.12.15", "2.12.14", "2.12.13", "2.12.12", "2.12.11", "2.12.10")
          case (2, 13) => Seq("2.13.10", "2.13.9", "2.13.8", "2.13.7", "2.13.6", "2.13.5", "2.13.4", "2.13.3")
          case (3, _) => Seq("3.2.0", "3.1.3", "3.1.2", "3.1.1", "3.1.0", "3.0.2", "3.0.1", "3.0.0")
        }
        .toSeq
        .flatten
    },
    crossTarget := target.value / s"scala-${scalaVersion.value}",
    crossVersion := CrossVersion.full,
    Compile / unmanagedSourceDirectories ++= {
      val sourceDir = (Compile / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value).collect {
        case (3, 0) => sourceDir / s"scala-3.0"
        case (3, minor) => sourceDir / s"scala-3.1+"
      }
    },
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value).collect {
        case (2, _) => "org.scala-lang" % "scala-compiler" % scalaVersion.value
        case (3, _) => "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value).collect {
        case (2, 12) => "-Ywarn-unused-import"
        case (2, 13) => "-Wunused:imports"
      }
    }
  )

lazy val scala3StepFilter = project
  .in(file("scala-3-step-filter"))
  .disablePlugins(SbtJdiTools)
  .settings(
    name := "scala-debug-step-filter",
    scalaVersion := Dependencies.scala3,
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query" % "0.1.1",
      "org.scala-lang" %% "tasty-core" % scalaVersion.value,
      Dependencies.munit % Test,
      Dependencies.coursier.cross(CrossVersion.for3Use2_13) % Test
    ),
    test / logBuffered := false
  )
