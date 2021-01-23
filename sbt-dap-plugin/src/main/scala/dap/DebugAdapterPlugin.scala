package dap

import sbt._
import sbt.internal.server.{ServerCallback, ServerHandler, ServerIntent}
import com.google.gson.{Gson, GsonBuilder}
import sbt.internal.bsp._
import sbt.internal.protocol.JsonRpcRequestMessage
import sbt.internal.langserver.ErrorCodes
import sjsonnew.shaded.scalajson.ast.unsafe.JValue

import scala.util.{Failure, Success}
import sbt.internal.util.complete.Parser
import sbt.internal.util.complete.Parsers
import scala.util.Try
import sjsonnew.support.scalajson.unsafe.{ CompactPrinter, Converter, Parser => JsonParser }
import scala.concurrent.ExecutionContext
import java.nio.file.Path
import java.io.File
import dap.{DebugSessionDataKind => DataKind}

object DebugAdapterPlugin extends sbt.AutoPlugin {
  private final val DebugSessionStart: String = "debugSession/start"

  object autoImport {
    val dapMainClassSession = inputKey[StateTransform]("Start a debug session for running a scala main class").withRank(KeyRanks.DTask)
    val dapTestSuitesSession = inputKey[StateTransform]("Start a debug session for running test suites").withRank(KeyRanks.DTask)
    val dapAttachRemoteSession = inputKey[StateTransform]("Start a debug session on a remote process").withRank(KeyRanks.DTask)

    // attribute key for storing the instance of [[DebugServer]]
    // each build target can have only one buildServer
    val debugServers = AttributeKey[Map[BuildTargetIdentifier, DebugServer]]("debugServers")
  }
  import autoImport._
  import dap.codec.JsonProtocol._
  import sbt.internal.bsp.codec.JsonProtocol._

  private val jsonParser: Parser[Result[JValue]] = (Parsers.any *)
    .map(_.mkString)
    .map(JsonParser.parseFromString)
    .map(_.toEither.left.map(Error.parseError))

  private implicit val executionContext =
    ExecutionContext.fromExecutor(DebugServerThreadPool.executor)

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    Keys.serverHandlers += {
      val loadedBuild = Keys.loadedBuild.value
      val configMap = loadedBuild.allProjectRefs
          .flatMap { case (_, p) => p.configurations }
          .distinct
          .map(c => ConfigKey(c.name) -> c)
          .toMap
      debugSessionStartHandler(Keys.bspWorkspace.value, configMap)
    },
    dapMainClassSession := {
      val target = Keys.bspTargetIdentifier.value
      val javaHome = Keys.javaHome.value
      val workingDirectory = Keys.baseDirectory.value
      val envVars = Keys.envVars.value
      val converter = Keys.fileConverter.value
      val analyses = ??? // get all analysis transitively
      val sbtLogger = Keys.streams.value.log
      val result = for {
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
          runJVMOptions = params.arguments,
          connectInput = false,
          envVars = envVars
        )
        val runner = new MainClassSbtDebuggeeRunner(
          target,
          forkOptions,
          params.`class`,
          params.arguments,
          analyses,
          converter,
          sbtLogger
        )
      }
    
      
      val runner = ???
      // new MainClassSbtDebuggeeRunner(
      //   target,
      //   Keys.javaHome.value,
      //   Keys.forkOptions.value,
      //   Keys.options,???
      // )
      val logger: Logger = ???
      startServer(target, runner, logger)
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
            case DebugSessionDataKind.ScalaMainClass => dapMainClassSession.key
            case DebugSessionDataKind.ScalaTestSuites => dapTestSuitesSession.key
            case DebugSessionDataKind.ScalaAttachRemote => dapAttachRemoteSession.key
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

  private def startServer(targetId: BuildTargetIdentifier, runner: DebuggeeRunner, logger: Logger): StateTransform = {
    StateTransform { state =>
      val allServers = state.get(debugServers).getOrElse(Map())
      // if there is a server for this target then close it
      allServers.get(targetId).foreach(_.close())
      val server = DebugServer(runner, logger)
      server.start()
      // not sure if it's possible to respond in a state event
      state.respondEvent(DebugSessionAddress(server.uri))
      state.put(debugServers, allServers.updated(targetId, server))
    }
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