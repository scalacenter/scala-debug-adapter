name := "dap-test"
version := "0.1"
scalaVersion := "2.12.12"
libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.6"
testFrameworks += new TestFramework("utest.runner.Framework")
Test / fork := true