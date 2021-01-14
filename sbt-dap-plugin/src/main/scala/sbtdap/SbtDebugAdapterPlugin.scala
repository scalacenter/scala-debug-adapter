package sbtdap

import sbt.{Keys, Project, State}
import sbt.internal.server.{ServerCallback, ServerHandler, ServerIntent}
import com.google.gson.{Gson, GsonBuilder}
import sjsonnew.support.scalajson.unsafe.Converter
import dap.DebuggeeRunner
import sbt.internal.bsp._
import sbt.internal.protocol.JsonRpcRequestMessage
import sbt.internal.langserver.ErrorCodes
import sjsonnew.shaded.scalajson.ast.unsafe.JValue

import scala.util.{Failure, Success}

object SbtDebugAdapterPlugin extends sbt.AutoPlugin {
  private implicit val gson: Gson = new GsonBuilder().setPrettyPrinting().create()

  override def projectSettings = Seq(
    Keys.serverHandlers += ServerHandler { callback: ServerCallback =>
      import callback._

      ServerIntent.request {
        case r: JsonRpcRequestMessage if r.method == "debugSession/start" => {
          import sbt.internal.bsp.codec.DebugAdapterJsonProtocol._

          handleDebugSessionStart(r, ???, ???) match {
            case Left(protoError) => jsonRpcRespondError(Some(r.id), protoError.code, protoError.message)
            case Right(debugAddress) => jsonRpcRespond(debugAddress, Some(r.id))
          }
          ()
        }
      }
    }
  )

  private def handleDebugSessionStart(
    r: JsonRpcRequestMessage,
    projects: Seq[Project],
    state: State
  ): BspResponse[DebugSessionAddress] = {
    for {
      params: DebugSessionParams <- getDebugSessionParams(r)
      runner <- SbtDebuggeeRunner.inferDebuggeeRunner(params, projects, state)
      // TODO: This needs to be refactored into a Future/callbacks/etc
      address: DebugSessionAddress = DebugSessionAddress(runner.getURI)
    } yield address
  }

  private def getDebugSessionParams(r: JsonRpcRequestMessage): BspResponse[DebugSessionParams] = {
    r.params match {
      case None => Left(JsonRpcResponse.paramsMissing(r.method))
      case Some(value: JValue) =>
        import sbt.internal.bsp.codec.DebugAdapterJsonProtocol._
        Converter.fromJson[DebugSessionParams](value) match {
          case Success(params: DebugSessionParams) => Right(params)
          case Failure(err) => Left(JsonRpcResponse.invalidParams(err.getMessage))
        }
    }
  }

  object autoImport

  private implicit class FilterableEither[E, T](val x: Either[E, T]) extends AnyVal {
    def withFilter(p: T => Boolean): Either[E, T] = x
  }
}