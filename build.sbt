import sbt.CrossVersion
import sbt.Keys.crossVersion
import scala.collection.mutable

def isRelease() =
  System.getenv("GITHUB_REPOSITORY") == "scalacenter/scala-debug-adapter" &&
    Option(System.getenv("GITHUB_WORKFLOW")).exists(_.contains("Release"))

def isCI = System.getenv("CI") != null

inThisBuild(
  Seq(
    organization := "ch.epfl.scala",
    homepage := Some(url("https://github.com/scalacenter/scala-debug-adapter")),
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := Developers.list,
    version ~= { dynVer => "4.0.3"
      // if (isRelease) dynVer
      // else "4.1.0-SNAPSHOT" // only for local publishing
    },
    resolvers += Resolver.mavenLocal,
    compile / javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(
    javaDebug,
    core212,
    core213,
    tests212,
    sbtPlugin,
    expressionCompiler212,
    expressionCompiler213,
    expressionCompiler30,
    expressionCompiler33,
    expressionCompiler34,
    decoder3
  )
  .settings(
    publish / skip := true
  )

lazy val javaDebug = project
  .in(file("modules/java-debug/com.microsoft.java.debug.core"))
  .settings(
    name := "com-microsoft-java-debug-core",
    crossPaths := false,
    autoScalaLibrary := false,
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-lang3" % "3.14.0",
      "com.google.code.gson" % "gson" % "2.10.1",
      "io.reactivex.rxjava2" % "rxjava" % "2.2.21",
      "org.reactivestreams" % "reactive-streams" % "1.0.4",
      "commons-io" % "commons-io" % "2.15.1",
      "junit" % "junit" % "4.13.2" % Test,
      "org.easymock" % "easymock" % "5.2.0" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test
    ),
    Test / fork := true,
    version := Option(System.getenv("JAVA_DEBUG_VERSION")).getOrElse("0.34.0-SNAPSHOT"),
    Compile / doc / sources := Seq.empty // workaround for this issue : https://github.com/scalameta/metals/issues/5096
  )

lazy val core212 = core.jvm(Dependencies.scala212)
lazy val core213 = core.jvm(Dependencies.scala213)
lazy val core = projectMatrix
  .in(file("modules/core"))
  .jvmPlatform(
    Seq(Dependencies.scala212, Dependencies.scala213, Dependencies.scala31Plus),
    Seq.empty,
    p => p.dependsOn(javaDebug)
  )
  .enablePlugins(SbtJdiTools, BuildInfoPlugin)
  .settings(
    name := "scala-debug-adapter",
    scalacOptionsSettings,
    libraryDependencies ++= List(
      Dependencies.scalaReflect(scalaVersion.value),
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.sbtTestAgent,
      Dependencies.scalaMeta
    ),
    libraryDependencies += onScalaVersion(
      scala212 = Dependencies.scalaCollectionCompat,
      scala213 = Dependencies.scalaParallelCollection,
      scala3 = Dependencies.scalaParallelCollection
    ).value,
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.action("organization")(organization.value),
      BuildInfoKey.action("version")(version.value),
      BuildInfoKey.action("expressionCompilerName")((LocalProject("expressionCompiler212") / name).value),
      BuildInfoKey.action("decoderName")((LocalProject("decoder3") / name).value),
      BuildInfoKey.action("scala212")(Dependencies.scala212),
      BuildInfoKey.action("scala213")(Dependencies.scala213),
      BuildInfoKey.action("scala30")(Dependencies.scala30),
      BuildInfoKey.action("scala31Plus")(Dependencies.scala31Plus),
      BuildInfoKey.action("scala34Plus")(Dependencies.scala34Plus)
    ),
    buildInfoPackage := "ch.epfl.scala.debugadapter"
  )

lazy val tests212 = tests.jvm(Dependencies.scala212)
lazy val tests213 = tests.jvm(Dependencies.scala213)
lazy val tests3 = tests.jvm(Dependencies.scala31Plus)
lazy val tests = projectMatrix
  .in(file("modules/tests"))
  .jvmPlatform(scalaVersions = Seq(Dependencies.scala212, Dependencies.scala213, Dependencies.scala31Plus))
  .settings(
    name := "scala-debug-adapter-test",
    libraryDependencies ++= Seq(
      Dependencies.munit,
      Dependencies.coursier.cross(CrossVersion.for3Use2_13),
      Dependencies.coursierJvm.cross(CrossVersion.for3Use2_13)
    ),
    scalacOptionsSettings,
    PgpKeys.publishSigned := {},
    publish := {},
    Test / baseDirectory := (ThisBuild / baseDirectory).value / "modules" / "tests",
    testOptionsSettings
  )
  .dependsOn(core)

lazy val sbtPlugin = project
  .in(file("modules/sbt-plugin"))
  .enablePlugins(SbtPlugin, ContrabandPlugin, JsonCodecPlugin)
  .settings(
    name := "sbt-debug-adapter",
    scalaVersion := Dependencies.scala212,
    scalacOptionsSettings,
    sbtVersion := "1.4.9",
    scriptedSbt := "1.5.5",
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false,
    scriptedDependencies := scriptedDependencies
      .dependsOn(
        publishLocal,
        core212 / publishLocal,
        tests212 / publishLocal,
        expressionCompiler212 / publishLocal,
        decoder3 / publishLocal
      )
      .value
  )
  .dependsOn(core212)

lazy val expressionCompiler212 = expressionCompiler.finder(scala212Axis)(true)
lazy val expressionCompiler213 = expressionCompiler.finder(scala213Axis)(true)
lazy val expressionCompiler30 = expressionCompiler.finder(scala30Axis)(true)
lazy val expressionCompiler33 = expressionCompiler.finder(scala31PlusAxis)(true)
lazy val expressionCompiler34 = expressionCompiler.finder(scala34PlusAxis)(true)
lazy val expressionCompiler = projectMatrix
  .in(file("modules/expression-compiler"))
  .customRow(true, Seq(scala212Axis, VirtualAxis.jvm), p => p.dependsOn(tests212 % Test))
  .customRow(true, Seq(scala213Axis, VirtualAxis.jvm), p => p.dependsOn(tests213 % Test))
  .customRow(true, Seq(scala30Axis, VirtualAxis.jvm), p => p)
  .customRow(true, Seq(scala31PlusAxis, VirtualAxis.jvm), p => p.dependsOn(tests3 % Test))
  .customRow(true, Seq(scala34PlusAxis, VirtualAxis.jvm), p => p.dependsOn(tests3 % Test))
  .settings(
    name := "scala-expression-compiler",
    crossScalaVersions ++= CrossVersion
      .partialVersion(scalaVersion.value)
      .collect {
        case (2, 12) =>
          Seq("2.12.19", "2.12.18", "2.12.17", "2.12.16", "2.12.15", "2.12.14", "2.12.13", "2.12.12", "2.12.11")
        case (2, 13) =>
          Seq("2.13.13", "2.13.12", "2.13.11", "2.13.10", "2.13.9", "2.13.8", "2.13.7", "2.13.6", "2.13.5", "2.13.4")
        case (3, 0) => Seq("3.0.2", "3.0.1", "3.0.0")
        case (3, 1 | 2 | 3) => Seq("3.3.3", "3.3.2", "3.3.1", "3.3.0", "3.2.2", "3.2.1", "3.2.0", "3.1.3", "3.1.2", "3.1.1", "3.1.0")
        case (3, _) => Seq("3.5.0-RC1", "3.4.2", "3.4.1", "3.4.0")
        // format: on
      }
      .toSeq
      .flatten,
    crossScalaVersions := crossScalaVersions.value.distinct,
    libraryDependencies ++= Seq(Dependencies.munit % Test) ++ onScalaVersion(
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
        case (3, 1 | 2 | 3) => sourceDir / s"scala-3.1+"
        case (3, _) => sourceDir / s"scala-3.4+"
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val sourceDir = (Test / sourceDirectory).value
      CrossVersion.partialVersion(scalaVersion.value).collect { case (3, 3) =>
        sourceDir / "scala-3.3"
      }
    },
    Compile / doc / sources := Seq.empty,
    libraryDependencies += Dependencies.scalaCompiler(scalaVersion.value),
    scalacOptionsSettings
  )

lazy val decoder3: Project = project
  .in(file("modules/decoder"))
  .disablePlugins(SbtJdiTools)
  .dependsOn(tests3 % Test)
  .settings(
    name := "scala-debug-decoder",
    scalaVersion := Dependencies.scala34Plus,
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= Seq(
      "ch.epfl.scala" %% "tasty-query" % "1.3.0",
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.munit % Test
    ),
    testOptionsSettings
  )

lazy val testOptionsSettings = Def.settings(
  Test / fork := true,
  // do not use sbt logger, otherwise the output of a test only appears at the end of the suite
  Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "+l"),
  Test / testOptions := (Test / testOptions)
    .dependsOn(
      // break cyclic reference
      LocalProject("expressionCompiler212") / publishLocal,
      LocalProject("expressionCompiler213") / publishLocal,
      LocalProject("expressionCompiler31Plus") / publishLocal,
      LocalProject("expressionCompiler34Plus") / publishLocal,
      LocalProject("decoder3") / publishLocal
    )
    .value
)

lazy val scalacOptionsSettings = Def.settings(
  scalacOptions ++= onScalaVersion(
    scala212 = Seq("-Xsource:3", "-Ywarn-unused-import", "-deprecation"),
    scala213 = Seq("-Xsource:3", "-Wunused:imports", "-deprecation"),
    scala3 = Seq("-deprecation")
  ).value
)

// Custom Scala version axis with minor version as suffix
lazy val scala212Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala212, "212")
lazy val scala213Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala213, "213")
lazy val scala30Axis = VirtualAxis.ScalaVersionAxis(Dependencies.scala30, "30")
lazy val scala31PlusAxis = VirtualAxis.ScalaVersionAxis(Dependencies.scala31Plus, "31Plus")
lazy val scala34PlusAxis = VirtualAxis.ScalaVersionAxis(Dependencies.scala34Plus, "34Plus")

def onScalaVersion[T](scala212: T, scala213: T, scala3: T) = Def.setting {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => scala212
    case Some((2, 13)) => scala213
    case Some((3, _)) => scala3
    case _ => ???
  }
}
