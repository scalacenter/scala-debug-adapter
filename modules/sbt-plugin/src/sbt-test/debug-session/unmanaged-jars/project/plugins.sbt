val pluginVersion = sys.props
  .get("plugin.version")
  .getOrElse {
    sys.error(
      """|The system property 'plugin.version' is not defined.
         |Specify this property using the scriptedLaunchOpts -D.
         |""".stripMargin
    )
  }

addSbtPlugin("ch.epfl.scala" % "sbt-debug-adapter" % pluginVersion)
libraryDependencies += "ch.epfl.scala" %% "scala-debug-adapter-test" % pluginVersion

// this plugin adds the tools.jar from the JDK as an unmanaged jar; sbt-jdi-tools is sbt 1 only
libraryDependencies ++= {
  if (sbtBinaryVersion.value == "1.0")
    Seq(
      Defaults
        .sbtPluginExtra("com.github.sbt" % "sbt-jdi-tools" % "1.2.0", sbtBinaryVersion.value, scalaBinaryVersion.value)
    )
  else Nil
}
