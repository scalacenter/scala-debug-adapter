package sbtdap

import ch.epfl.scala.bsp
import sbt.{Keys, Project, State}
import sbt.internal.server.{ServerCallback, ServerHandler, ServerIntent}
import com.google.gson.{Gson, GsonBuilder}
import dap.DebuggeeRunner
import scala.util.{Failure, Success}
import sjsonnew.shaded.scalajson.ast.unsafe.JValue

object SbtDebugAdapterPlugin extends sbt.AutoPlugin {
  private implicit val gson: Gson = new GsonBuilder().setPrettyPrinting().create()

  import scala.meta.jsonrpc.{Response => JsonRpcResponse}
  private type ProtocolError = JsonRpcResponse.Error

  override def projectSettings = Seq(
    Keys.serverHandlers += ServerHandler { callback: ServerCallback =>
      import callback._
      //import sjsonnew.BasicJsonProtocol._
      import sbt.internal.protocol.JsonRpcRequestMessage
      ServerIntent.request {
        case r: JsonRpcRequestMessage if r.method == "debugSession/start" => {

          val parsedParams: Either[ProtocolError, bsp.DebugSessionParams] = r.params match {
            case None => Left(JsonRpcResponse.invalidParams(s"param is expected on '${r.method}' method."))
            case Some(paramsJson: JValue) =>
              import io.circe.parser.decode
              decode[bsp.DebugSessionParams](gson.toJson(paramsJson)) match {
                case Right(params) => Right(params)
                case Left(error) => Left(JsonRpcResponse.invalidParams(error.getMessage))
              }
          }


          parsedParams.map { params =>
            SbtBspServerContext.startSessionDebug(logger, params)
          } match  {
            case Left(err: ProtocolError) =>
              import err.error._
              jsonRpcRespondError(Some(err.id.toString), code.value, message)
            case Right(runner) =>
              // TODO: the native JsonFormat objects just got merged into sbt: https://github.com/sbt/sbt/pull/6226
              //val debugAddress = bsp.DebugSessionAddress(uri = ???)
              // need JsonFormat[bsp.DebugSessionAddress]
              //jsonRpcRespond(debugAddress, Some(r.id))

          }

          ()
        }
      }
    }
  )



  object autoImport
}