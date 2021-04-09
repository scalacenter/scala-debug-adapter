val pluginVersion = sys.props.get("plugin.version")
  .getOrElse {
    sys.error(
      """|The system property 'plugin.version' is not defined.
         |Specify this property using the scriptedLaunchOpts -D.
         |""".stripMargin
    )
  }

addSbtPlugin("ch.epfl.scala" % "sbt-debug-adapter" % pluginVersion)
libraryDependencies += "ch.epfl.scala" %% "debug-adapter-test-client" % pluginVersion