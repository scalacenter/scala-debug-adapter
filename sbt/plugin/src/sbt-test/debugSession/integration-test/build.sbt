import ch.epfl.scala.debug.DebugAdapterPlugin

val root = project.in(file("."))
  .configs(IntegrationTest)
  .settings(
    scalaVersion := "2.12.12",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % IntegrationTest,
    testFrameworks += new TestFramework("utest.runner.Framework"),
    Defaults.itSettings,
    inConfig(IntegrationTest)(DebugAdapterPlugin.testSettings)
  )
