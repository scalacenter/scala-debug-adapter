package dap

import sbt._
import sbt.internal.server.{ServerHandler, ServerIntent}
import sbt.internal.bsp._
import sbt.internal.protocol.JsonRpcRequestMessage
import sjsonnew.shaded.scalajson.ast.unsafe.JValue
import scala.util.{Failure, Success}
import sbt.internal.inc.Analysis
import sbt.internal.util.complete.Parser
import sbt.internal.util.complete.Parsers
import sjsonnew.support.scalajson.unsafe.{CompactPrinter, Converter, Parser => JsonParser}
import scala.concurrent.ExecutionContext
import java.io.File
import dap.{DebugSessionDataKind => DataKind}
import dap.codec.JsonProtocol._
import sbt.Defaults.createTestRunners
import sbt.Keys.{parallelExecution, tags, testOptions}
import sbt.internal.bsp.codec.JsonProtocol._
import sbt.internal.util.Terminal
import sbt.io.IO
import sbt.testing.Framework
import scala.collection.mutable
import sjsonnew.BasicJsonProtocol
import sbt.Tests.InProcess
import sbt.Tests.SubProcess
import sbt.Tests.Setup
import sbt.Tests.Cleanup
import sbt.Tests.Argument

object DebugAdapterPlugin extends sbt.AutoPlugin {
  private final val DebugSessionStart: String = "debugSession/start"

  // each build target can only have one debug server
  private val debugServers = mutable.Map[BuildTargetIdentifier, DebugServer]()

  object autoImport {
    val startMainClassDebugSession = inputKey[URI]("Start a debug session for running a scala main class").withRank(KeyRanks.DTask)
    val startTestSuitesDebugSession = inputKey[URI]("Start a debug session for running test suites").withRank(KeyRanks.DTask)
    val startRemoteDebugSession = inputKey[URI]("Start a debug session on a remote process").withRank(KeyRanks.DTask)
    
    val stopDebugSession = taskKey[Unit]("Stop the current debug session").withRank(KeyRanks.DTask)
  }
  import autoImport._

  private val jsonParser: Parser[Result[JValue]] = (Parsers.any *)
    .map(_.mkString)
    .map(JsonParser.parseFromString)
    .map(_.toEither.left.map(Error.parseError))

  private implicit val executionContext =
    ExecutionContext.fromExecutor(DebugServerThreadPool.executor)

  override def trigger = allRequirements

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
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

  override def projectSettings: Seq[Def.Setting[_]] = {
    inConfig(Compile)(runSettings) ++ inConfig(Test)(testSettings)
  }

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
   *     inConfig(IntegrationTest)(DebugAdapterPlugin.testSettings)
   *   )
   * }}}
   */
  def testSettings: Seq[Def.Setting[_]] = Seq(
    startTestSuitesDebugSession := testSuitesSessionTask.evaluated,
    startRemoteDebugSession := remoteSessionTask.value,
    stopDebugSession := stopSessionTask.value
  )

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
            case DebugSessionDataKind.ScalaMainClass => startMainClassDebugSession.key
            case DebugSessionDataKind.ScalaTestSuites => startTestSuitesDebugSession.key
            case DebugSessionDataKind.ScalaAttachRemote => startRemoteDebugSession.key
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
    val envVars = Keys.envVars.value
    val converter = Keys.fileConverter.value
    val classpath = Keys.fullClasspath.value
    val sbtLogger = Keys.streams.value.log
    val state = Keys.state.value

    val runner = for {
      json <- jsonParser.parsed
      params <- Converter.fromJson[ScalaMainClass](json).toEither.left.map { cause =>
        Error.invalidParams(s"expected data of kind ${DataKind.ScalaMainClass}: ${cause.getMessage}")
      }
    } yield {
      val classpathOption = Attributed.data(classpath).map(_.getAbsolutePath).mkString(File.pathSeparator)
      val forkOptions = ForkOptions(
        javaHome = javaHome,
        outputStrategy = None,
        bootJars = Vector.empty[File],
        workingDirectory = Option(workingDirectory),
        runJVMOptions = params.jvmOptions,
        connectInput = false,
        envVars = envVars
      )

      new MainClassRunner(
        target,
        forkOptions,
        classpath,
        params.`class`,
        params.arguments,
        converter,
        sbtLogger
      )
    }

    runner match {
      case Left(error) =>
        Keys.state.value.respondError(error.code, error.message)
        throw new MessageOnlyException(error.message)
      case Right(runner) =>
        startServer(state, target, runner, sbtLogger)
    }
  }

  private def testSuitesSessionTask: Def.Initialize[InputTask[URI]] = Def.inputTask {
    val target = Keys.bspTargetIdentifier.value
    
    val testGrouping = (Keys.test / Keys.testGrouping).value
    val defaultForkOpts = Keys.forkOptions.value
    val classpath = (Keys.test / Keys.fullClasspath).value
    val frameworks = Keys.loadedTestFrameworks.value
    val testLoader = Keys.testLoader.value
    val testExec = (Keys.test / Keys.testExecution).value
    val parallelExec = (Keys.test / Keys.testForkedParallel).value
    val state = Keys.state.value

    val converter = Keys.fileConverter.value
    val sbtLogger = Keys.streams.value.log

    import BasicJsonProtocol._
    val runner = for {
      json <- jsonParser.parsed
      testSuites <- Converter.fromJson[Array[String]](json).toEither.left.map { cause =>
        Error.invalidParams(
          s"expected data of kind ${DataKind.ScalaTestSuites}: ${cause.getMessage}"
        )
      }
      testGroup <- testGrouping.filter { g =>
        val testSet = g.tests.map(_.name).toSet
        testSuites.forall(testSet.contains)
      }.headOption.toRight {
        Error.invalidParams("all tests are not in the same group")
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
        classpath,
        setups,
        cleanups,
        parallel,
        testRunners,
        testDefinitions,
        converter,
        sbtLogger
      )
    }

    runner match {
      case Left(error) =>
        state.respondError(error.code, error.message)
        throw new MessageOnlyException(error.message)
      case Right(runner) =>
        startServer(state, target, runner, sbtLogger)
    }
  }

  private def remoteSessionTask: Def.Initialize[Task[URI]] = Def.task {
    val target = Keys.bspTargetIdentifier.value
    
    val classpath = (Keys.test / Keys.fullClasspath).value
    val state = Keys.state.value

    val converter = Keys.fileConverter.value
    val logger = Keys.streams.value.log

    val runner = new AttachRemoteRunner(target, classpath, converter, logger)
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
        import dap.codec.JsonProtocol._
        Converter.fromJson[DebugSessionParams](value) match {
          case Success(params: DebugSessionParams) => Right(params)
          case Failure(err) => Left(Error.invalidParams(err.getMessage))
        }
    }
  }

  private implicit class FilterableEither[E, T](val x: Either[E, T]) extends AnyVal {
    def withFilter(p: T => Boolean): Either[E, T] = x
  }
}