val pluginVersion = sys.props.get("plugin.version")
  .getOrElse {
    sys.error(
      """|The system property 'plugin.version' is not defined.
         |Specify this property using the scriptedLaunchOpts -D.
         |""".stripMargin
    )
  }

// required for adopt@1.8
addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")
addSbtPlugin("ch.epfl.scala" % "sbt-debug-adapter" % pluginVersion)
libraryDependencies += "ch.epfl.scala" %% "scala-debug-adapter-test-client" % pluginVersion
