inThisBuild(
  List(
    onLoadMessage := s"Welcome to scala-debug-adapter ${version.value}",
    scalaVersion := Dependencies.scala212,
    //crossScalaVersions := List(Dependencies.scala213, Dependencies.scala212, Dependencies.scala211),
  )
)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "scala-debug-adapter",
    libraryDependencies ++= List(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      Dependencies.asm,
      Dependencies.asmUtil,
      Dependencies.bsp4j,
      Dependencies.bsp4s,
      Dependencies.javaDebug,
      Dependencies.monix,
      Dependencies.sbtTestInterface,
      Dependencies.scalameta,
      Dependencies.zinc,
    )
  )
