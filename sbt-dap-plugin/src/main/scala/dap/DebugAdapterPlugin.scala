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
import sbt.internal.bsp.codec.JsonProtocol._
import scala.collection.mutable
object DebugAdapterPlugin extends sbt.AutoPlugin {
  import SbtLoggerAdapter._
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

  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(configSettings)

  private def configSettings: Seq[Def.Setting[_]] = Seq(
    startMainClassDebugSession := startMainClassTask.evaluated,
    stopDebugSession := {
      val target = Keys.bspTargetIdentifier.value
      debugServers.get(target).foreach(_.close())
      val _ = debugServers.remove(target)
    }
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
          val dataStr = CompactPrinter(params.data.get)
          val project = scope.project.toOption.get.asInstanceOf[ProjectRef].project
          val config = configMap(scope.config.toOption.get).id

          // the project and config that correspond to the target identifier
          // the task that will create the debugee and start the debugServer
          // the data containing the debuggee parameters
          s"$project / $config / $task $dataStr"
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

  private def startMainClassTask: Def.Initialize[InputTask[URI]] = Def.inputTask {
    val target = Keys.bspTargetIdentifier.value
    val javaHome = Keys.javaHome.value
    val workingDirectory = Keys.baseDirectory.value
    val envVars = Keys.envVars.value
    val converter = Keys.fileConverter.value
    val classpath = Keys.fullClasspath.value
    val analyses = classpath
      .flatMap(_.metadata.get(Keys.analysis))
      .map { case a: Analysis => a }
    val sbtLogger = Keys.streams.value.log
    val state = Keys.state.value

    val runner = for {
      json <- jsonParser.parsed
      params <- Converter.fromJson[ScalaMainClass](json).toEither.left.map { cause =>
        Error.invalidParams(s"expected data of kind ${DataKind.ScalaMainClass}: ${cause.getMessage}")
      }
    } yield {
      val classpathOption = Attributed.data(classpath).map(_.getAbsolutePath).mkString(File.pathSeparator)
      val jvmOpts = Vector("-classpath", classpathOption) ++ 
        Some("-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,quiet=n") ++
        params.jvmOptions
      val forkOptions = ForkOptions(
        javaHome = javaHome,
        outputStrategy = None,
        bootJars = Vector.empty[File],
        workingDirectory = Option(workingDirectory),
        runJVMOptions = jvmOpts,
        connectInput = false,
        envVars = envVars
      )

      new MainClassSbtDebuggeeRunner(
        target,
        forkOptions,
        params.`class`,
        params.arguments,
        analyses,
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

  private def startServer(state: State, target: BuildTargetIdentifier, runner: DebuggeeRunner, logger: Logger): URI = {
    // if there is a server for this target then close it
    debugServers.get(target).foreach(_.close())
    val server = DebugServer(runner, logger)
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