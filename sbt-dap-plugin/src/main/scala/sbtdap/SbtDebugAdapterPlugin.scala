package sbtdap

import sbt.{Keys, Project, State}
import sbt.internal.server.{ServerCallback, ServerHandler, ServerIntent}
import com.google.gson.{Gson, GsonBuilder}
import dap.DebuggeeRunner
import scala.util.{Failure, Success}
import sjsonnew.shaded.scalajson.ast.unsafe.JValue

object SbtDebugAdapterPlugin extends sbt.AutoPlugin {
  private implicit val gson: Gson = new GsonBuilder().setPrettyPrinting().create()

  override def projectSettings = Seq(
    Keys.serverHandlers += ServerHandler { callback: ServerCallback =>
      import callback._
      //import sjsonnew.BasicJsonProtocol._
      import sbt.internal.protocol.JsonRpcRequestMessage
      ServerIntent.request {
        case r: JsonRpcRequestMessage if r.method == "debugSession/start" => {
          ???
          ()
        }
      }
    }
  )

  object autoImport
}