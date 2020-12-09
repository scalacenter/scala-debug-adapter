# scala-debug-adapter

Implementation of the [Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/) for Scala

# Project Goals

The library should be a depedency that [Bloop](https://github.com/scalacenter/bloop), [SBT](https://github.com/sbt/sbt), [Mill](https://github.com/lihaoyi/mill), or other [Build Server Protocol](https://github.com/build-server-protocol/build-server-protocol) can import and use to implement [debugSession/start](https://github.com/build-server-protocol/build-server-protocol/blob/master/bsp4s/src/main/scala/ch/epfl/scala/bsp/endpoints/Endpoints.scala#L72) in BSP.

# References

- [Bloop Debuggig Referece](scalacenter.github.io/bloop/docs/debugging-reference)
- [Microsoft DAP for Java](https://github.com/microsoft/vscode-java-debug)
- [DebugAdapterProvider](https://github.com/build-server-protocol/build-server-protocol/issues/145)
- [Bloop's DAP](https://github.com/scalacenter/bloop/tree/master/frontend/src/main/scala/bloop/dap) &  [Bloop's DAP  Tests](https://github.com/scalacenter/bloop/tree/master/frontend/src/test/scala/bloop/dap)

# History

- [Origial project discussion](https://github.com/scalameta/metals-feature-requests/issues/168)
