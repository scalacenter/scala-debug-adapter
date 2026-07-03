// required for adopt@1.8; sbt-jdi-tools is published for sbt 1 only, and the JDK ships JDI on 9+
libraryDependencies ++= {
  if (sbtBinaryVersion.value == "1.0")
    Seq(Defaults.sbtPluginExtra("com.github.sbt" % "sbt-jdi-tools" % "1.2.0", sbtBinaryVersion.value, scalaBinaryVersion.value))
  else Nil
}
