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

// this plugin add the tools.jar from the JDK into the classpath as an unmanaged jar.
addSbtPlugin("com.github.sbt" % "sbt-jdi-tools" % "1.2.0")
