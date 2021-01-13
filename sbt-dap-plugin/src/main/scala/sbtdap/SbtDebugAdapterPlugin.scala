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

          ()
        }
      }
    }
  )



  object autoImport
}