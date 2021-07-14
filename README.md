# scala-debug-adapter
[![build-badge][]][build]
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/ch.epfl.scala/sbt-debug-adapter/badge.svg)](https://maven-badges.herokuapp.com/maven-central/ch.epfl.scala/sbt-debug-adapter)

[build]:       https://github.com/scalacenter/scala-debug-adapter/actions?query=branch%3Amain+workflow%3A%22Continuous+Integration%22
[build-badge]: https://github.com/scalacenter/scala-debug-adapter/workflows/Continuous%20Integration/badge.svg?branch=main

The scala-debug-adapter is a server-side implementation of the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) for the Scala language running on the JVM platform.
It is based on and extends the [microsoft/java-debug](https://github.com/microsoft/java-debug) implementation.

The project originated in the [Bloop](https://github.com/scalacenter/bloop) repository, it is now released independently so that it can be used in other build tools of the Scala ecosystem.
For instance, the [sbt-debug-adapter](#sbt-debug-adapter) is an sbt plugin that provide sbt with the debug adapter capability. 

## Usage

You can add the `scala-debug-adapter` as a dependency in your `build.sbt`:
```scala
// build.sbt
scalaVersion := "2.12.4",
libraryDependencies += "ch.epfl.scala" %% "scala-debug-adapter" % "1.1.2"
```

The `scala-debug-adapter` expects the Java Debug Interface to be class loaded.
While it is always the case with Java 9, you probably want to use the `sbt-jdi-tools` to be able to run on Java 8 as well:

```scala
// project/plugins.sbt
addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")
```

You can start a debug server by providing your own intance of `DebuggeeRunner` and `Logger`.
Examples of `DebuggeeRunner` can be found [here](https://github.com/scalacenter/scala-debug-adapter/blob/main/sbt/plugin/src/main/scala/ch/epfl/scala/debugadapter/sbtplugin/internal/SbtDebuggeeRunner.scala).

```scala
val runner = new MyDebuggeeRunner()
val logger = new MyLogger()
val server = DebugServer(runner, logger)
server.start()
// use `server.uri` for the client to connect
```

# sbt-debug-adapter

The `sbt-debug-adapter` is an sbt plugin compatible with sbt `1.4.0` or greater.
It provides the sbt server with the BSP `debugSession/start` endpoint to start a Scala DAP server.

The specification of the `debugSession/start` endpoint can be found in the [Bloop documentation](https://scalacenter.github.io/bloop/docs/debugging-reference).

## Usage
As a global plugin use `~/.sbt/1.0/plugins/plugins.sbt`, otherwise add to your local project (e.g. `project/plugins.sbt`):

```scala
// project/plugins.sbt
addSbtPlugin("ch.epfl.scala" % "sbt-debug-adapter" % "1.0.0")
```

Again the JDI tools must be class loaded, this time by sbt itself.
To do so you can use the `sbt-jdi-tools` plugin in the meta project (it goes to `project/project/plugins.sbt`).

```scala
// project/project/plugins.sbt
addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")
```

# Development

## test-client

The `test-client` module is used internally to test the debug server.
It contains the [TestDebugClient](./test-client/src/main/scala/ch/epfl/scala/debugadapter/testing/TestDebugClient.scala), a minimal debug client that is used to communicate with the real server via socket.

# References

- [Bloop Debugging Referece](https://scalacenter.github.io/bloop/docs/debugging-reference)
- [Microsoft DAP for Java](https://github.com/microsoft/vscode-java-debug)
- [DebugAdapterProvider](https://github.com/build-server-protocol/build-server-protocol/issues/145)

# History

- [Origial project discussion](https://github.com/scalameta/metals-feature-requests/issues/168)
