# Contributing

This project follows the [Scala Code of Conduct](https://scala-lang.org/conduct/).

## Requirements

You need the following tools:

- [git](https://git-scm.com/)
- Java 8 or greater
- the sbt build tool
- one of the Scala code editor:
  - [VSCode](https://code.visualstudio.com/) with [Metals](https://marketplace.visualstudio.com/items?itemName=scalameta.metals) (recommended)
  - [IntelliJ IDEA Community Edition](https://www.jetbrains.com/idea/) with the [Scala plugin](https://plugins.jetbrains.com/plugin/1347-scala).

To install Java and sbt we recommend using Coursier (the official Scala installer) and following these [instructions](https://docs.scala-lang.org/getting-started/index.html).

## Setting Up

1. Clone the repository and its submodule

```
$ git clone git@github.com:scalacenter/scala-debug-adapter.git
$ cd scala-debug-adapter
$ git submodule update --init
```

2. Start the sbt shell in the terminal and compile

```
$ sbt
sbt:root> compile
```

3. Import the project in your code editor

Congratulations! You are now ready to start coding.

## Project Structure

All the modules are in the `modules` folder. They are composed of:
- `java-debug`: A fork of [microsoft/java-debug](https://github.com/microsoft/java-debug) on which the scala-debug-adapter depends.
The `java-debug` project was originally a Maven project.
The `scala-debug-adapter` only depends on the `com.microsoft.java.debug.core` module and we compile it directly using sbt.
So we can ignore the Maven configuration and the other modules.
- `core`: The scala-debug-adapter itself which contains the `DebugServer` and all its configuration classes.
- `expression-compiler`: An extension of the Scala compiler for compiling a Scala expression in the context of debugging (the current stack frame of a paused JVM) into a class file that can be loaded and invoked.
The `expression-compiler` is cross-compiled on all supported minor Scala versions.
It is class-loaded by the `ExpressionCompiler` object from `core`.
- `scala-3-step-filter`: The implementation of `StepFilter` for Scala 3.
The `scala-3-step-filter`, compiled with Scala 3, is class-loaded by the `Scala3StepFilter` object from `core` in a separate class loader.
(The Scala 2 step filter is in `core` directly because it is compiled in Scala 2 and does not need to be class-loaded separately)
- `tests`: The test module. It contains the test infrastructure, in `tests/src/main`, and the unit tests, in `tests/src/test`.
- `sbt-plugin`: The sbt plugin that allows sbt to start the DAP server.

## Unit-Testing the debugger

Most of the tests are hosted in the `tests` module.

The `tests/src/main` folder contains the testing infrastructure:
- the `TestingDebugClient` class: a client of the debug server used for testing
- the `TestingDebuggee` object: to configure, compile and debug some Scala processes as debuggees for testing the debugger.
- the `DebugTestSuite` abstract class: an extension of `munit.TestSuite` that allows describing a debug scenario and checking that it happens as described.

As instance, we can write and run this test:
```scala
package ch.epfl.scala.debugadapter

import ch.epfl.scala.debugadapter.testfmk.*

class MyDebugTests extends DebugTestSuite {
  private val scalaVersion: ScalaVersion = ScalaVersion.`3.3`

  test("my simple test") {
    val source =
      """|package example
         |
         |object Main {
         |  def main(args: Array[String]): Unit = {
         |    val x = 42
         |    val msg = s"Hello, $x"
         |    println(msg)
         |  }
         |}
         |""".stripMargin
    // configure the debuggee: a Scala 3 main class `example.Main`
    implicit val debuggee: TestingDebuggee = TestingDebuggee.mainClass(source, "example.Main", scalaVersion)
    check(
      // check that the debuggee stops on a breakpoint on line 7
      Breakpoint(7),
      // check that the evaluation of expression x is 42  
      Evaluation.success("x", 42),
      // check that a stepIn request goes into the Predef$.println method
      StepIn.method("Predef$.println(Object)")
    )
  }
}
```

The organization of the tests is:
- `DebugServerTests`: general tests about starting and exiting debug sessions on all kinds of debugees
- `ScalaDebugTests`: general tests about inspecting threads, stack frames and variables in Scala programs
- `ScalaEvaluationTests`: tests about the Scala expression evaluator
- `SourceBreakpointTests`: tests about the different kinds of source breakpoints: conditional breakpoints, hit count breakpoints, and log points.
- `StepFilterTests`: tests about stepping in and out of Scala methods.

The tests are quite heavy to run because every test compiles a small Scala program, creates a debug server that starts a fresh JVM to run the program, and executes a debugging scenario.
All of this is repeated on a few Scala versions.

Running all the test can take more than 30 minutes.
We recommend that locally you run only some tests using the sbt `testOnly` task or using your IDE:

```
sbt:root> testOnly ch.epfl.scala.debugadapter.Scala31PlusEvaluationTests
```

## Running the debugger locally

You can install and start a local version of the scala-debug-adapter using Metals.

1. In sbt, check out the version of the scala-debug-adapter project.

```
sbt:root> version
...
[info] 3.0.2-SNAPSHOT
```

In the following steps we will use `3.0.2-SNAPSHOT`.

2. Publish the scala-debug-adapter locally.

```
sbt:root> publishLocal
```

3. Open an sbt project in VS Code with Metals.
As we will run the debugger in this project, it must contain a main class or some test suites.

4. Open or create the `project/plugins.sbt` file.
This is where we can configure the version of the scala-debug-adapter that we want to use.

5. Configure the `sbt-debug-adapter` plugin in the `project/plugins.sbt` file.
Here you should use the local version that you just published, instead of `"3.0.2-SNAPSHOT"`.

```scala
addSbtPlugin("ch.epfl.scala" % "sbt-debug-adapter" % "3.0.2-SNAPSHOT")
```

6. Switch the build server. In VS Code, open the command palette, run `Metals: switch build server` and select `sbt`.

7. Since we changed the build file, we may need to reload sbt and re-import the build. You can do so by opening the command palette and running the `Metals: Import build` command, or by clicking on `Import build` in Metals'tab.

8. Start the debugger:
  - click on the `debug` lens on top of a main method
  - or right-click on a test and run `Debug test`
  - or add a `Scala` debug configuration in the `.vscode/launch.json` file

## Releasing

### Releasing the full scala-debug-adapter

To publish a new release you can go to the [release page](https://github.com/scalacenter/scala-debug-adapter/releases/new), to create a new tag and generate the release notes.

**The release is not triggered automatically.**

You then need to go to the [Full Release (manual)](https://github.com/scalacenter/scala-debug-adapter/actions/workflows/release-full.yml) workflow and trigger it.
Click on `Run Workflow`, choose the freshly created tag, and a new version for java-debug-core.
To find the next version of java-debug-core you can see all versions in [Maven Central](https://repo1.maven.org/maven2/ch/epfl/scala/com-microsoft-java-debug-core/).

### Releasing the expression-compiler on a specific Scala version

The Expression Compiler, that is used in the debug console to evaluate expression, is cross-compiled on each Scala version.
If a version of the Expression Compiler is missing for some version of Scala, you can release it using the [Release Expression Compiler (manual)](https://github.com/scalacenter/scala-debug-adapter/actions/workflows/release-expression-compiler.yml) workflow.
This workflow was introduced in tag v3.0.9, it does not work on previous tags.

Click on `Run Workflow`, choose the latest tag, or the one we use in Bloop and Metals, and specify the Scala version.

This workflow triggers the tests, to check that the new Scala version did not break anything, and the release of the Scala Expression Compiler.
