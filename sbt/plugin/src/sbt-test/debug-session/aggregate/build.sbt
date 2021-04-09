ThisBuild / scalaVersion := "2.13.4"

lazy val common = project.in(file("common"))

lazy val root = project.in(file("."))
  .aggregate(common)
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2",
  )
