

import java.io.File

inThisBuild(
  Seq(
    organization := "ch.epfl.scala",
    version := "1.0.0-SNAPSHOT",
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    scalaVersion := Dependencies.scala212,
    // resolvers ++= Seq(
    //   Resolver.bintrayRepo("scalacenter", "releases")
    // )
  )
)

lazy val core = project
  .enablePlugins(SbtJdiTools, BuildInfoPlugin)
  .in(file("core"))
  .settings(
    name := "scala-debug-adapter",
    libraryDependencies ++= List(
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug,
      Dependencies.utest % Test,
      Dependencies.scalaCompiler % Test,
      Dependencies.io % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    // Test / javaOptions += "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=1044",
    Test / fork := true,
    // build info is used to locate the library dependencies from the tests
    addBuildInfoToConfig(Test),
    buildInfoKeys := Seq[BuildInfoKey](
      BuildInfoKey.map(scalaInstance) { case (_, scalaInstance) => 
        "scalaLibraries" -> scalaInstance.libraryJars.mkString(File.pathSeparator)
      }
    )
  )

lazy val sbtPlugin = project
  .in(file("sbt/plugin"))
  .enablePlugins(SbtPlugin, ContrabandPlugin, JsonCodecPlugin)
  .settings(
    name := "sbt-debug-adapter",
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedDependencies := {
      publishLocal.value
      (core / publishLocal).value
      (testAgent / publishLocal).value 
    }
  )
  .dependsOn(core, testAgent)

// copy of https://github.com/sbt/sbt/tree/develop/testing/agent/src/main/java/sbt
lazy val testAgent = project
  .in(file("sbt/test-agent"))
  .settings(
    name := "debug-test-agent",
    autoScalaLibrary := false,
    crossPaths := false,
    libraryDependencies += Dependencies.sbtTestInterface
  )
