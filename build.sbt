

import java.io.File

inThisBuild(
  List(
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    scalaVersion := Dependencies.scala212,
    Keys.resolvers := {
      val oldResolvers = Keys.resolvers.value
      val sonatypeStaging = Resolver.sonatypeRepo("staging")
      val scalametaResolver = Resolver.bintrayRepo("scalameta", "maven")
      val scalacenterResolver = Resolver.bintrayRepo("scalacenter", "releases")
      (oldResolvers :+ sonatypeStaging :+ scalametaResolver :+ scalacenterResolver).distinct
    },
    //crossScalaVersions := List(Dependencies.scala213, Dependencies.scala212, Dependencies.scala211),
  )
)

val bloopVersion = "1.4.5" // "1.4.6-15-209c2a5c" // "1.4.6"

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
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-dap-plugin"
  )
  .dependsOn(core)


lazy val bloopDap = project
  .in(file("bloop-dap"))
  .settings(
    name := "bloop-scala-debug-adapter",
    libraryDependencies ++= List(
      "ch.epfl.scala" %% "bloop-frontend" % bloopVersion
    )
  )
  .dependsOn(core)