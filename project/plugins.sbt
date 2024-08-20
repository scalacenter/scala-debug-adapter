// required for adopt@1.8
addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")

addSbtPlugin("org.scala-sbt" % "sbt-contraband" % "0.5.3")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.6.1")
// force version to fix https://github.com/sbt/sbt-pgp/issues/199
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.12.0")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.10.0")
