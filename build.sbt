

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
  .in(file("sbt-dap-plugin"))
  .enablePlugins(SbtPlugin, ContrabandPlugin, JsonCodecPlugin)
  .settings(
    name := "sbt-dap-plugin",
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    scriptedBufferLog := false,
  )
  .dependsOn(core)


lazy val bloopDap = project
  .in(file("bloop-dap"))
  .settings(
    name := "bloop-scala-debug-adapter",
    libraryDependencies ++= Seq(Dependencies.bloopFrontend)
  )
  .dependsOn(core)