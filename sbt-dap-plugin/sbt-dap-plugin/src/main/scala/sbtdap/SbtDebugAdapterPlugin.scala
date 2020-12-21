package sbtdap

import ch.epfl.scala.bsp
import sbt.{Keys, Project, State}
import sbt.internal.server.{ServerCallback, ServerHandler, ServerIntent}
import com.google.gson.{Gson, GsonBuilder}
import dap.DebuggeeRunner
import io.circe.{Decoder, parser}
import io.circe.syntax._
import sbt.internal.protocol.JsonRpcRequestMessage

object SbtDebugAdapterPlugin extends sbt.AutoPlugin {
  private implicit val gson: Gson = new GsonBuilder().setPrettyPrinting().create()

  import scala.meta.jsonrpc.{JsonRpcClient, Response => JsonRpcResponse,
  private type ProtocolError = JsonRpcResponse.Error

  private case class InvalidRequest(execId: Option[String], code: Long, message: String) extends ProtocolError

  override def projectSettings = Seq(
    Keys.serverHandlers += ServerHandler { callback: ServerCallback =>
      import callback._
      //import sjsonnew.BasicJsonProtocol._
      import sbt.internal.protocol.{JsonRpcRequestMessage}
      ServerIntent.request {
        case r: JsonRpcRequestMessage if r.method == "debugSession/start" =>
          val params: bsp.DebugSessionParams = ???
          val projects: Seq[Project] = ???
          val state: State = ???

          val result: Either[ProtocolError, DebuggeeRunner] = params.dataKind match {
            case bsp.DebugSessionParamsDataKind.ScalaMainClass =>
              convert[bsp.ScalaMainClass](params, r, main => SbtDebuggeeRunner.forMainClass(projects, main, state))
            case bsp.DebugSessionParamsDataKind.ScalaTestSuites =>
              convert[List[String]](params, r, filters => SbtDebuggeeRunner.forTestSuite(projects, filters, state))
            case bsp.DebugSessionParamsDataKind.ScalaAttachRemote =>
              Right(SbtDebuggeeRunner.forAttachRemote(state))
            case dataKind => Left(InvalidRequest(s"Unsupported data kind: $dataKind"))
          }


          result match {
            case Left(error) =>
              import error._
              jsonRpcRespondError(execId, code, message)
            case Right(runner) =>
              val debugAddress = bsp.DebugSessionAddress(uri = runner.)
              jsonRpcRespond(debugAddress, Some(r.id))
          }



/*
  def startDebugSession(
      params: bsp.DebugSessionParams
  ): BspEndpointResponse[bsp.DebugSessionAddress] = {

    ifInitialized(None) { (state, logger) =>
      JavaRuntime.loadJavaDebugInterface match {
        case Failure(exception) =>
          val message = JavaRuntime.current match {
            case JavaRuntime.JDK => Feedback.detectedJdkWithoutJDI(exception)
            case JavaRuntime.JRE => Feedback.detectedUnsupportedJreForDebugging(exception)
          }
          Task.now((state, Left(JsonRpcResponse.internalError(message))))

        case Success(_) =>
          mapToProjects(params.targets, state) match {
            case Left(error) =>
              // Log the mapping error to the user via a log event + an error status code
              logger.error(error)
              Task.now((state, Left(JsonRpcResponse.invalidRequest(error))))
            case Right(mappings) =>
              // FIXME: Add origin id to DAP request
              compileProjects(mappings, state, Nil, None, logger).flatMap {
                case (state, Left(error)) =>
                  Task.now((state, Left(error)))
                case (state, Right(result)) if result.statusCode != bsp.StatusCode.Ok =>
                  Task.now(
                    (state, Left(JsonRpcResponse.internalError("Compilation not successful")))
                  )
                case (state, Right(_)) =>
                  val projects = mappings.map(_._2)
                  inferDebuggeeRunner(projects, state) match {
                    case Right(runner) =>
                      val startedServer = DebugServer.start(runner, logger, ioScheduler)
                      val listenAndUnsubscribe = startedServer.listen
                        .runOnComplete(_ => backgroundDebugServers -= startedServer)(ioScheduler)
                      backgroundDebugServers += startedServer -> listenAndUnsubscribe

                      startedServer.address.map {
                        case Some(uri) => (state, Right(new bsp.DebugSessionAddress(uri.toString)))
                        case None =>
                          val error = JsonRpcResponse.internalError("Failed to start debug server")
                          (state, Left(error))
                      }

                    case Left(error) =>
                      Task.now((state, Left(error)))
                  }
              }
          }
      }
    }
 */
          ()
      }
    }
  )

  private def convert[A: Decoder](
    params: bsp.DebugSessionParams,
    request: JsonRpcRequestMessage,
    f: A => Either[String, DebuggeeRunner]
  ): Either[ProtocolError, DebuggeeRunner] = {
    params.data.as[A] match {
      case Left(error) =>
        Left(InvalidRequest(Some(request.id), 1, error.getMessage()))
      case Right(params) =>
        f(params) match {
          case Right(adapter) => Right(adapter)
          case Left(error) => Left(InvalidRequest(Some(request.id), 1, error))
        }
    }
  }

  object autoImport
}