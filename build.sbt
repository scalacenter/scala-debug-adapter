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
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := Developers.list,
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
    core212,
    tests212,
    sbtPlugin,
    expressionCompiler212,
    expressionCompiler213,
    expressionCompiler30,
    expressionCompiler32,
    scala3StepFilter
  )
  .settings(
    publish / skip := true
  )

lazy val core212 = core.jvm(Dependencies.scala212)
lazy val core = projectMatrix
  .in(file("core"))
  .jvmPlatform(scalaVersions = Seq(Dependencies.scala212, Dependencies.scala213, Dependencies.scala32))
  .enablePlugins(SbtJdiTools, BuildInfoPlugin)
  .settings(
    name := "scala-debug-adapter",
    scalacOptionsSetting,
    libraryDependencies ++= List(
      Dependencies.scalaReflect(scalaVersion.value),
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug,
      Dependencies.sbtTestAgent
    ),
    libraryDependencies += onScalaVersion(
      scala212 = Dependencies.scalaCollectionCompat,
      scala213 = Dependencies.scalaParallelCollection,
      scala3 = Dependencies.scalaParallelCollection
    ).value,
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.action("organization")(organization.value),
      BuildInfoKey.action("version")(version.value),
      BuildInfoKey.action("expressionCompilerName")((expressionCompiler212 / name).value),
      BuildInfoKey.action("scala3StepFilterName")((LocalProject("scala3StepFilter") / name).value),
      BuildInfoKey.action("scala212")(Dependencies.scala212),
      BuildInfoKey.action("scala213")(Dependencies.scala213),
      BuildInfoKey.action("scala30")(Dependencies.scala30),
      BuildInfoKey.action("scala32")(Dependencies.scala32)
    ),
    buildInfoPackage := "ch.epfl.scala.debugadapter"
  )

lazy val tests212 = tests.jvm(Dependencies.scala212)
lazy val tests3 = tests.jvm(Dependencies.scala32)
lazy val tests = projectMatrix
  .in(file("tests"))
  .jvmPlatform(scalaVersions = Seq(Dependencies.scala212, Dependencies.scala213, Dependencies.scala32))
  .settings(
    name := "scala-debug-adapter-test",
    libraryDependencies ++= Seq(
      Dependencies.munit,
      Dependencies.coursier.cross(CrossVersion.for3Use2_13),
      Dependencies.coursierJvm.cross(CrossVersion.for3Use2_13)
    ),
    scalacOptionsSetting,
    PgpKeys.publishSigned := {},
    publish := {},
    // Test / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044",
    Test / fork := true,
    Test / baseDirectory := (ThisBuild / baseDirectory).value / "tests",
    // do not use sbt logger, otherwise the output of a test only appears at the end of the suite
    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "+l"),
    Test / testOptions := (Test / testOptions)
      .dependsOn(
        expressionCompiler212 / publishLocal,
        expressionCompiler213 / publishLocal,
        expressionCompiler30 / publishLocal,
        expressionCompiler32 / publishLocal,
        // break cyclic reference
        LocalProject("scala3StepFilter") / publishLocal
      )
      .value
  )
  .dependsOn(core)

lazy val sbtPlugin = project
  .in(file("sbt-plugin"))
  .enablePlugins(SbtPlugin, ContrabandPlugin, JsonCodecPlugin)
  .settings(
    name := "sbt-debug-adapter",
    scalaVersion := Dependencies.scala212,
    scalacOptionsSetting,
    sbtVersion := "1.4.9",
    scriptedSbt := "1.5.5",
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false,
    scriptedDependencies := scriptedDependencies
      .dependsOn(publishLocal, core212 / publishLocal, tests212 / publishLocal)
      .value
  )
  .dependsOn(core212)

lazy val expressionCompiler212 = expressionCompiler.finder(scala212Axis)(true)
lazy val expressionCompiler213 = expressionCompiler.finder(scala213Axis)(true)
lazy val expressionCompiler30 = expressionCompiler.finder(scala30Axis)(true)
lazy val expressionCompiler32 = expressionCompiler.finder(scala32Axis)(true)
lazy val expressionCompiler = projectMatrix
  .in(file("expression-compiler"))
  .customRow(true, Seq(scala212Axis, VirtualAxis.jvm), identity[Project] _)
  .customRow(true, Seq(scala213Axis, VirtualAxis.jvm), identity[Project] _)
  .customRow(true, Seq(scala30Axis, VirtualAxis.jvm), identity[Project] _)
  .customRow(true, Seq(scala32Axis, VirtualAxis.jvm), identity[Project] _)
  .settings(
    name := "scala-expression-compiler",
    crossScalaVersions ++= CrossVersion
      .partialVersion(scalaVersion.value)
      .collect {
        case (2, 12) => Seq("2.12.17", "2.12.16", "2.12.15", "2.12.14", "2.12.13", "2.12.12", "2.12.11", "2.12.10")
        case (2, 13) => Seq("2.13.10", "2.13.9", "2.13.8", "2.13.7", "2.13.6", "2.13.5", "2.13.4", "2.13.3")
        case (3, 0) => Seq("3.0.2", "3.0.1", "3.0.0")
        case (3, _) => Seq("3.2.0", "3.1.3", "3.1.2", "3.1.1", "3.1.0")
      }
      .toSeq
      .flatten,
    crossScalaVersions := crossScalaVersions.value.distinct,
    libraryDependencies ++= onScalaVersion(
      scala212 = Some(Dependencies.scalaCollectionCompat),
      scala213 = None,
      scala3 = None
    ).value,
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
    libraryDependencies += Dependencies.scalaCompiler(scalaVersion.value),
    scalacOptionsSetting
  )

lazy val scala3StepFilter: Project = project
  .in(file("scala-3-step-filter"))
  .disablePlugins(SbtJdiTools)
  .dependsOn(tests3 % Test)
  .settings(
    name := "scala-debug-step-filter",
    scalaVersion := Dependencies.scala32,
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query" % "0.1.2",
      "org.scala-lang" %% "tasty-core" % scalaVersion.value,
      Dependencies.munit % Test
    ),
    Test / fork := true
  )

lazy val scalacOptionsSetting = Def.settings(
  scalacOptions ++= onScalaVersion(
    scala212 = Seq("-Xsource:3", "-Ywarn-unused-import", "-deprecation"),
    scala213 = Seq("-Xsource:3", "-Wunused:imports", "-deprecation"),
    scala3 = Seq("-deprecation")
  ).value
)

// Custom Scala version axis with minor version as suffix
lazy val scala212Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala212, "2_12")
lazy val scala213Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala213, "2_13")
lazy val scala30Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala30, "3_0")
lazy val scala32Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala32, "3_2")

def onScalaVersion[T](scala212: T, scala213: T, scala3: T) = Def.setting {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => scala212
    case Some((2, 13)) => scala213
    case Some((3, _)) => scala3
    case _ => ???
  }
}
