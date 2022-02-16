import sbt.CrossVersion
import sbt.Keys.crossVersion

import java.io.File

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
    scalacOptions ++= Seq(
      "-Xsource:3",
      "-Ywarn-unused-import"
    ),
    version ~= { dynVer =>
      if (isRelease) dynVer
      else "2.1.0-SNAPSHOT" // only for local publishing
    },
    libraryDependencies ++= {
      if (isCI) Nil
      else List(Dependencies.pprint)
    }
    // resolvers += Resolver.mavenLocal
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(core, sbtPlugin, expressionCompiler)
  .settings(
    PgpKeys.publishSigned := {},
    publishLocal := {}
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(SbtJdiTools, BuildInfoPlugin)
  .settings(
    name := "scala-debug-adapter",
    libraryDependencies ++= List(
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug,
      Dependencies.sbtTestAgent,
      Dependencies.utest % Test,
      Dependencies.coursier % Test,
      Dependencies.coursierJvm % Test
    ),
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.action("expressionCompilerOrganization")(
        (expressionCompiler / organization).value
      ),
      BuildInfoKey.action("expressionCompilerName")(
        (expressionCompiler / name).value
      ),
      BuildInfoKey.action("expressionCompilerVersion")(
        (expressionCompiler / version).value
      )
    ),
    buildInfoPackage := "ch.epfl.scala.debugadapter",
    testFrameworks += new TestFramework("utest.runner.Framework"),
    // Test / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044",
    Test / fork := true
  )
  .dependsOn(testClient % Test)

lazy val testClient = project
  .in(file("test-client"))
  .settings(
    name := "debug-adapter-test-client",
    libraryDependencies ++= List(
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug
    )
  )

lazy val sbtPlugin = project
  .in(file("sbt/plugin"))
  .enablePlugins(SbtPlugin, ContrabandPlugin, JsonCodecPlugin)
  .settings(
    name := "sbt-debug-adapter",
    sbtVersion := "1.4.9",
    scriptedSbt := "1.5.5",
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false,
    scriptedDependencies := {
      publishLocal.value
      (core / publishLocal).value
      (testClient / publishLocal).value
    }
  )
  .dependsOn(core)

lazy val expressionCompiler = project
  .in(file("expression-compiler"))
  .settings(
    name := "scala-expression-compiler",
    crossScalaVersions := Seq(
      "3.0.2",
      "3.0.1",
      "3.0.0",
      "2.13.8",
      "2.13.7",
      "2.13.6",
      "2.13.5",
      "2.13.4",
      "2.13.3",
      "2.12.15",
      "2.12.14",
      "2.12.13",
      "2.12.12",
      "2.12.11",
      "2.12.10"
    ),
    Compile / doc := {
      // Scaladoc fails
      new File("")
    },
    crossTarget := target.value / s"scala-${scalaVersion.value}",
    crossVersion := CrossVersion.full,
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, _)) =>
          List("org.scala-lang" % "scala-compiler" % scalaVersion.value)
        case _ => Nil
      }
    },
    scalacOptions -= "-Ywarn-unused-import"
  )
