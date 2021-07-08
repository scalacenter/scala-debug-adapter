package ch.epfl.scala.debugadapter.sbtplugin

import ch.epfl.scala.debugadapter.sbtplugin.internal._
import ch.epfl.scala.debugadapter.sbtplugin.internal.JsonProtocol._
import ch.epfl.scala.debugadapter._

import sbt.Tests._
import sbt._
import sbt.internal.bsp.BuildTargetIdentifier
import sbt.internal.protocol.JsonRpcRequestMessage
import sbt.internal.server.{ServerHandler, ServerIntent}
import sbt.internal.util.complete.{Parser, Parsers}
import sjsonnew.BasicJsonProtocol
import sjsonnew.shaded.scalajson.ast.unsafe.JValue
import sjsonnew.support.scalajson.unsafe.{CompactPrinter, Converter, Parser => JsonParser}

import java.io.File
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.{Failure, Success}
import scala.collection.concurrent.TrieMap

object DebugAdapterPlugin extends sbt.AutoPlugin {
  private final val DebugSessionStart: String = "debugSession/start"
  private type Result[A] = Either[Error, A]
  
  private object DataKind {
    final val ScalaMainClass: String = "scala-main-class"
    final val ScalaTestSuites: String = "scala-test-suites"
    final val ScalaAttachRemote: String = "scala-attach-remote"
  }

  // each build target can only have one debug server
  private val debugServers = TrieMap[BuildTargetIdentifier, DebugServer]()

  private val jsonParser: Parser[Result[JValue]] = Parsers.any.*
    .map(_.mkString)
    .map(JsonParser.parseFromString)
    .map(_.toEither.left.map(Error.parseError))

  private implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(DebugServerThreadPool.executor)

  object autoImport {
    val startMainClassDebugSession = inputKey[URI]("Start a debug session for running a scala main class").withRank(KeyRanks.DTask)
    val startTestSuitesDebugSession = inputKey[URI]("Start a debug session for running test suites").withRank(KeyRanks.DTask)
    val startRemoteDebugSession = taskKey[URI]("Start a debug session on a remote process").withRank(KeyRanks.DTask)
    
    val stopDebugSession = taskKey[Unit]("Stop the current debug session").withRank(KeyRanks.DTask)
  }
  import autoImport._

  override def trigger = allRequirements

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    Keys.initialize := {
      val _ = Keys.initialize.value
      assertJDITools(Keys.sLog.value)
    },
    Keys.serverHandlers += {
      val loadedBuild = Keys.loadedBuild.value
      val configMap = loadedBuild.allProjectRefs
          .flatMap { case (_, p) => p.configurations }
          .distinct
          .map(c => ConfigKey(c.name) -> c)
          .toMap
      debugSessionStartHandler(Keys.bspWorkspace.value, configMap)
    }
  )

  override def projectSettings: Seq[Def.Setting[_]] = Def.settings(
    inConfig(Compile)(runSettings),
    inConfig(Test)(testSettings),
    startMainClassDebugSession / Keys.aggregate := false,
    startTestSuitesDebugSession / Keys.aggregate := false,
    startRemoteDebugSession / Keys.aggregate := false,
    stopDebugSession / Keys.aggregate := false
  )

  def runSettings: Seq[Def.Setting[_]] = Seq(
    startMainClassDebugSession := mainClassSessionTask.evaluated,
    startRemoteDebugSession := remoteSessionTask.value,
    stopDebugSession := stopSessionTask.value
  )

  /**
   * Can be used to add debugSession/start support in the IntegrationTest config
   * {{{
   * project
   *   .configs(IntegrationTest)
   *   .settings(
   *     Defaults.itSettings,
   *     inConfig(IntegrationTest)(DebugAdapterPlugin.testSettings)
   *   )
   * }}}
   */
  def testSettings: Seq[Def.Setting[_]] = Seq(
    startTestSuitesDebugSession := testSuitesSessionTask.evaluated,
    startRemoteDebugSession := remoteSessionTask.value,
    stopDebugSession := stopSessionTask.value
  )

  private def assertJDITools(logger: sbt.util.Logger): Unit = {
    val loader = getClass().getClassLoader()
    try {
      loader.loadClass("com.sun.jdi.Value")
    } catch {
      case c: ClassNotFoundException =>
        logger.warn("The sbt-debug-adapter cannot work because the JDI tools are not loaded.")
        logger.warn(
          """|
             |Consider adding the sbt-jdi-tools plugin to your `./project/project/plugins.sbt` file:
             |addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "x.y.z")
             |
             |""".stripMargin
        )
    }
  }

  private def debugSessionStartHandler(
    workspace: Map[BuildTargetIdentifier, Scope],
    configMap: Map[ConfigKey, Configuration]
  ): ServerHandler = ServerHandler { callback =>
    ServerIntent.request {
      case r: JsonRpcRequestMessage if r.method == DebugSessionStart =>
        val commandLine = for {
          params <- getDebugSessionParams(r)
          targetId <- singleBuildTarget(params)
          scope <- workspace.get(targetId).toRight(
            Error.invalidParams(s"$targetId is not a valid build target")
          )
          dataKind <- params.dataKind.toRight(
            Error.invalidParams(s"dataKind field is expected in '$DebugSessionStart' method")
          )
        } yield {
          val task = dataKind match {
            case DataKind.ScalaMainClass => startMainClassDebugSession.key
            case DataKind.ScalaTestSuites => startTestSuitesDebugSession.key
            case DataKind.ScalaAttachRemote => startRemoteDebugSession.key
          }
          val data = params.data.map(CompactPrinter.apply).getOrElse("")
          val project = scope.project.toOption.get.asInstanceOf[ProjectRef].project
          val config = configMap(scope.config.toOption.get).id

          // the project and config that correspond to the target identifier
          // the task that will create the debugee and start the debugServer
          // the data containing the debuggee parameters
          s"$project / $config / $task $data"
        }

        commandLine match {
          case Left(error) =>
            callback.jsonRpcRespondError(Some(r.id), error.code, error.message) 
          case Right(commandLine) =>
            // delegate the response to the command that will start the debuggee
            callback.appendExec(commandLine, Some(r.id))
        }
    }
  }

  private def stopSessionTask: Def.Initialize[Task[Unit]] = Def.task {
    val target = Keys.bspTargetIdentifier.value
    debugServers.get(target).foreach(_.close())
    val _ = debugServers.remove(target)
  }

  private def mainClassSessionTask: Def.Initialize[InputTask[URI]] = Def.inputTask {
    val target = Keys.bspTargetIdentifier.value
    val javaHome = Keys.javaHome.value
    val workingDirectory = Keys.baseDirectory.value
    val classPathEntries = InternalTasks.classPathEntries.value
    val envVars = Keys.envVars.value
    val logger = Keys.streams.value.log
    val state = Keys.state.value

    val runner = for {
      json <- jsonParser.parsed
      params <- Converter.fromJson[ScalaMainClass](json).toEither.left.map { cause =>
        Error.invalidParams(s"expected data of kind ${DataKind.ScalaMainClass}: ${cause.getMessage}")
      }
    } yield {
      val forkOptions = ForkOptions(
        javaHome = javaHome,
        outputStrategy = None,
        bootJars = Vector.empty[File],
        workingDirectory = Option(workingDirectory),
        runJVMOptions = params.jvmOptions,
        connectInput = false,
        envVars = envVars ++ params.environmentVariables
          .flatMap(_.split("=", 2).toList match {
            case key :: value :: Nil => Some(key -> value)
            case _                   => None
          })
          .toMap
      )

      new MainClassRunner(
        target,
        forkOptions,
        classPathEntries,
        params.`class`,
        params.arguments
      )
    }

    runner match {
      case Left(error) =>
        Keys.state.value.respondError(error.code, error.message)
        throw new MessageOnlyException(error.message)
      case Right(runner) =>
        startServer(state, target, runner, logger)
    }
  }

  private def testSuitesSessionTask: Def.Initialize[InputTask[URI]] = Def.inputTask {
    val target = Keys.bspTargetIdentifier.value
    val testGrouping = (Keys.test / Keys.testGrouping).value
    val defaultForkOpts = Keys.forkOptions.value
    val classPathEntries = InternalTasks.classPathEntries.value
    val frameworks = Keys.loadedTestFrameworks.value
    val testLoader = Keys.testLoader.value
    val testExec = (Keys.test / Keys.testExecution).value
    val parallelExec = (Keys.test / Keys.testForkedParallel).value
    val state = Keys.state.value
    val logger = Keys.streams.value.log

    import BasicJsonProtocol._
    val runner = for {
      json <- jsonParser.parsed
      testSuites <- Converter.fromJson[Array[String]](json).toEither.left.map { cause =>
        Error.invalidParams(
          s"expected data of kind ${DataKind.ScalaTestSuites}: ${cause.getMessage}"
        )
      }
      testGroup <- testGrouping.find { g =>
        val testSet = g.tests.map(_.name).toSet
        testSuites.forall(testSet.contains)
      }.toRight {
        Error.invalidParams("no matching test group")
      }
    } yield {
      val testDefinitions = testGroup.tests.filter(test => testSuites.contains(test.name))
      val forkOptions = testGroup.runPolicy match {
        case InProcess => defaultForkOpts
        case SubProcess(forkOpts) => forkOpts
      }

      val setups = testExec.options.collect { case setup @ Setup(_) => setup }
      val cleanups = testExec.options.collect { case cleanup @ Cleanup(_) => cleanup }
      val arguments = testExec.options.collect { case argument @ Argument(_, _) => argument }
      val parallel = testExec.parallel && parallelExec

      val testRunners = frameworks.map {
        case (name, framework) =>
          val args = arguments.collect {
            case Argument(None, args) => args
            case Argument(Some(tf), args) if tf == name => args
          }.flatten
          val mainRunner = framework.runner(args.toArray, Array.empty[String], testLoader)
          name -> mainRunner
      }

      new TestSuitesRunner(
        target,
        forkOptions,
        classPathEntries,
        setups,
        cleanups,
        parallel,
        testRunners,
        testDefinitions
      )
    }

    runner match {
      case Left(error) =>
        state.respondError(error.code, error.message)
        throw new MessageOnlyException(error.message)
      case Right(runner) =>
        startServer(state, target, runner, logger)
    }
  }

  private def remoteSessionTask: Def.Initialize[Task[URI]] = Def.task {
    val target = Keys.bspTargetIdentifier.value
    
    val classPathEntries = InternalTasks.classPathEntries.value
    val state = Keys.state.value
    val logger = Keys.streams.value.log

    val runner = new AttachRemoteRunner(target, classPathEntries)
    startServer(state, target, runner, logger)
  }

  private def startServer(
      state: State,
      target: BuildTargetIdentifier,
      runner: DebuggeeRunner,
      logger: sbt.util.Logger
  ): URI = {
    // if there is a server for this target then close it
    debugServers.get(target).foreach(_.close())
    val server = DebugServer(runner, new LoggerAdapter(logger))
    server.start()
    debugServers.update(target, server)
    state.respondEvent(DebugSessionAddress(server.uri))
    server.uri
  }

  private def singleBuildTarget(params: DebugSessionParams): Result[BuildTargetIdentifier] = {
    params.targets.size match {
      case 0 => Left(Error.invalidParams(s"one build target is expected in '$DebugSessionStart' method"))
      case 1 => Right(params.targets.head)
      case _ => Left(Error.invalidParams(s"multiple build targets is not supported in '$DebugSessionStart'"))
    }
  }

  private def getDebugSessionParams(r: JsonRpcRequestMessage): Result[DebugSessionParams] = {
    r.params match {
      case None => Left(Error.paramsMissing(r.method))
      case Some(value: JValue) =>
        Converter.fromJson[DebugSessionParams](value) match {
          case Success(params: DebugSessionParams) => Right(params)
          case Failure(err) => Left(Error.invalidParams(err.getMessage))
        }
    }
  }
}