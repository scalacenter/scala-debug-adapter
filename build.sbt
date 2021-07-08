import java.io.File

def isCI() = System.getenv("CI") != null

inThisBuild(
  Seq(
    organization := "ch.epfl.scala",
    homepage := Some(url("https://github.com/scalacenter/scala-debug-adapter")),
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := Developers.list,
    scalaVersion := Dependencies.scala212,
    version ~= { dynVer =>
      if (isCI()) dynVer
      else "2.0.0-SNAPSHOT" // only for local publishing
    },
    // resolvers += Resolver.mavenLocal
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(core, sbtPlugin, testAgent)
  .settings(
    PgpKeys.publishSigned := {},
    publishLocal := {}
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(SbtJdiTools)
  .settings(
    name := "scala-debug-adapter",
    libraryDependencies ++= List(
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.javaDebug,
      Dependencies.utest % Test,
      Dependencies.sbtIo % Test,
      Dependencies.coursier % Test
    ),
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
    Compile / generateContrabands / contrabandFormatsForType := ContrabandConfig.getFormats,
    scriptedLaunchOpts += s"-Dplugin.version=${version.value}",
    // scriptedBufferLog := false,
    scriptedDependencies := {
      publishLocal.value
      (core / publishLocal).value
      (testClient / publishLocal).value
      (testAgent / publishLocal).value
    }
  )
  .dependsOn(core, testAgent)

// copy of https://github.com/sbt/sbt/tree/develop/testing/agent/src/main/java/sbt
lazy val testAgent = project
  .in(file("sbt/test-agent"))
  .settings(
    name := "sbt-debug-test-agent",
    autoScalaLibrary := false,
    crossPaths := false,
    libraryDependencies += Dependencies.sbtTestInterface
  )
