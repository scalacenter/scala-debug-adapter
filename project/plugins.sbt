// required for adopt@1.8
addSbtPlugin("com.github.sbt" % "sbt-jdi-tools" % "1.2.0")

addSbtPlugin("org.scala-sbt" % "sbt-contraband" % "0.7.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.0")
// force version to fix https://github.com/sbt/sbt-pgp/issues/199
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.11.0")
