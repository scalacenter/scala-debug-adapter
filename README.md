# Scala Debug Adapter
[![build-badge][]][build-link] [![maven-badge][]][maven-link] [![scaladoc-badge][]][scaladoc-link]

[build-link]:     https://github.com/scalacenter/scala-debug-adapter/actions?query=branch%3Amain+workflow%3A%22Continuous+Integration%22
[build-badge]:    https://github.com/scalacenter/scala-debug-adapter/workflows/Continuous%20Integration/badge.svg?branch=main "Build Status"
[maven-link]:     https://maven-badges.herokuapp.com/maven-central/ch.epfl.scala/scala-debug-adapter_2.12
[maven-badge]:    https://maven-badges.herokuapp.com/maven-central/ch.epfl.scala/scala-debug-adapter_2.12/badge.svg "Maven Central"
[scaladoc-link]:  https://javadoc.io/doc/ch.epfl.scala/scala-debug-adapter_2.12/latest/scala-debug-adapter/index.html
[scaladoc-badge]: https://javadoc-badge.appspot.com/ch.epfl.scala/scala-debug-adapter_2.12.svg?label=scaladoc "Scaladoc"

Implementation of the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) for Scala

# Project Goals

The library should be a depedency that [Bloop](https://github.com/scalacenter/bloop), [sbt](https://github.com/sbt/sbt), [Mill](https://github.com/lihaoyi/mill), or other [BSP](https://github.com/build-server-protocol/build-server-protocol) servers can import and use to implement [`debugSession/start`](https://github.com/build-server-protocol/build-server-protocol/blob/master/bsp4s/src/main/scala/ch/epfl/scala/bsp/endpoints/Endpoints.scala#L72) in BSP.

## Documentation

Links:

- [Website](https://scalacenter.github.io/scala-debug-adapter/)
- [API documentation](https://scalacenter.github.io/scala-debug-adapter/api/)

# Development

* [TestDebugClient](./core/src/test/scala/dap/TestDebugClient.scala) is a minimal debug client that is used to communicate with the real server via socket.
* [MainDebuggeeRunner](core/src/test/scala/dap/MainDebuggeeRunner.scala) starts a real java process with debugging enabled so that the server can connect to it.

# References

- [Bloop Debuggig Referece](scalacenter.github.io/bloop/docs/debugging-reference)
- [Microsoft DAP for Java](https://github.com/microsoft/vscode-java-debug)
- [DebugAdapterProvider](https://github.com/build-server-protocol/build-server-protocol/issues/145)
- [Bloop's DAP](https://github.com/scalacenter/bloop/tree/master/frontend/src/main/scala/bloop/dap) &  [Bloop's DAP  Tests](https://github.com/scalacenter/bloop/tree/master/frontend/src/test/scala/bloop/dap)

# History

- [Origial project discussion](https://github.com/scalameta/metals-feature-requests/issues/168)

## Contributing

The Scala Debug Adapter project welcomes contributions from anybody wishing to participate.  All code or documentation that is provided must be licensed with the same license that Scala Debug Adapter is licensed with (Apache 2.0, see [LICENCE](./LICENSE.md)).

People are expected to follow the [Scala Code of Conduct](./CODE_OF_CONDUCT.md) when discussing Scala Debug Adapter on GitHub, Gitter channel, or other venues.

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted. For more information, check out the [contributor guide](./CONTRIBUTING.md).

## License

All code in this repository is licensed under the Apache License, Version 2.0.  See [LICENCE](./LICENSE.md).