val root = project.in(file("."))
  .settings(
    scalaVersion := "2.12.12",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6" % Test,
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
