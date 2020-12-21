package dap

import bloop.util.JavaRuntime
import ch.epfl.scala.bsp
import io.circe.Decoder
import monix.execution.{Cancelable, Scheduler}
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.meta.jsonrpc.{Response => JsonRpcResponse}
import scala.concurrent.ExecutionContext

object BspServerContext {
  private type ProtocolError = JsonRpcResponse.Error
  private type BspResponse[A] = Either[ProtocolError, A]


//  trait Task[A] {
//    def flatMap[B](a: A)(f: => Task[B]): Task[B]
//    def map[B](a: A)(f: A => B): Task[B]
//  }
//
//  implicit class TaskOps[+A](a: A)(implicit ev: Task[A]) {
//    def flatMap[B](f: => Task[B]): Task[B] = ev.flatMap(a)(f)
//    def map[B](f: A => B): Task[B] = ev.map(a)(f)
//  }
//
//  trait TaskFactory {
//    def apply[A](f: => A): Task[A]
//  }
//
//  object Task {
////    implicit val SbtTask = new Task[sbt.Task] {
////      def flatMap[A, B](a: A)(f: => Task[B]): Task[B] = a.flatMap(f)
////      def map[A, B](a: A)(f: A => B): Task[B] = b.flatMap(f)
////    }
//    private def monixTask[A](task: monix.eval.Task[A]): Task[A] = new Task[A] {
//      def flatMap[B](a: A)(f: => Task[B]): Task[B] = task.flatMap
//      def map[B](a: A)(f: A => B): Task[B]
//    }
//
//    implicit val MonixCompileResultTask = new Task[monix.eval.Task[A]] {
//      def flatMap[A, B](a: A)(f: => Task[B]): Task[B] = a.flatMap(f)
//      def map[A, B](a: A)(f: A => B): Task[B] = b.flatMap(f)
//    }
//
//  }
}
/**
 * Used to implement a build tool-specific BSP Debug Adapter
 */
trait BspServerContext {
  import BspServerContext._
  type Project
  type State
  type BspServerLogger <: bloop.logging.Logger

  protected def runnerFactory: DebuggeeRunnerFactory[Project, State]
//  protected def taskFactory: TaskFactory
  protected def state: State
  protected def buildToolName: String
  protected def ioScheduler: Scheduler
  protected implicit def executionContext: ExecutionContext


  final type ProjectMapping = (bsp.BuildTargetIdentifier, Project)

  private val backgroundDebugServers = TrieMap.empty[StartedDebugServer, Cancelable]

  // Build-specific methods to implement
  protected def getProjectFromBuildTargetIdentifier(
    target: bsp.BuildTargetIdentifier,
    state: State
  ): Either[String, Option[Project]]

  protected def compileProjects(
    userProjects: Seq[ProjectMapping],
    state: State,
    compileArgs: List[String],
    originId: Option[String],
    logger: BspServerLogger
  ): Future[Either[ProtocolError, bsp.CompileResult]]

  final def startSessionDebug(
    logger: BspServerLogger,
    params: bsp.DebugSessionParams
  ): Future[Either[ProtocolError, bsp.DebugSessionAddress]] =  {
    JavaRuntime.loadJavaDebugInterface match {
      case Failure(exception) =>
        val message = JavaRuntime.current match {
          case JavaRuntime.JDK => detectedJdkWithoutJDI(exception)
          case JavaRuntime.JRE => detectedUnsupportedJreForDebugging(exception)
        }
        //Task.now((state, Left(JsonRpcResponse.internalError(message))))
        Future.successful(Left(JsonRpcResponse.internalError(message)))

      case Success(_) =>
        mapToProjects(params.targets, state) match {
          case Left(error) =>
            // Log the mapping error to the user via a log event + an error status code
            logger.error(error)
            Future.successful(Left(JsonRpcResponse.invalidRequest(error)))
          case Right(mappings) =>
            // FIXME: Add origin id to DAP request
            // sbt.internal.server.BspCompileTask?
            compileProjects(mappings, state, Nil, None, logger).flatMap {
                //case (_, Left(error)) => Left(error)
                case Left(error) => Future.successful(Left(error))
                //case (_, Right(result: bsp.CompileResult)) if result.statusCode != bsp.StatusCode.Ok =>
                case Right(result: bsp.CompileResult) if result.statusCode != bsp.StatusCode.Ok =>
                  Future.successful(Left(JsonRpcResponse.internalError("Compilation not successful")))
                //case (state, Right(_)) =>
                case Right(_) =>
                  val projects = mappings.map(_._2)
                  inferDebuggeeRunner(params, projects, state) match {
                    case Right(runner) =>
                      val startedServer = DebugServer.start(runner, logger, ioScheduler)
                      val listenAndUnsubscribe = startedServer.listen
                        .runOnComplete(_ => backgroundDebugServers -= startedServer)(ioScheduler)
                      backgroundDebugServers += startedServer -> listenAndUnsubscribe

                      startedServer.address.map {
                        case Some(uri) => Right(new bsp.DebugSessionAddress(uri.toString))
                        case None =>
                          val error = JsonRpcResponse.internalError("Failed to start debug server")
                          Left(error)
                      }.runAsync(ioScheduler)

                    case Left(error) =>
                      Future.successful(Left(error))
                  }
            }
        }
    }
  }

  final def mapToProjects(targets: Seq[bsp.BuildTargetIdentifier], state: State): Either[String, Seq[ProjectMapping]] =
    if (targets.isEmpty) {
      Left("Empty build targets. Expected at least one build target identifier.")
    } else {
      val zero: Either[String, List[ProjectMapping]] = Right(Nil)
      targets.foldLeft(zero) { (acc, t) =>
        acc.flatMap(ms => mapToProject(t, state).map(m => m :: ms))
      }
    }

  final def mapToProject(target: bsp.BuildTargetIdentifier, state: State): Either[String, ProjectMapping] = {
    getProjectFromBuildTargetIdentifier(target, state) match {
      case Left(errorMsg) => Left(errorMsg)
      case Right(Some(project)) => Right((target, project))
      case Right(None) => Left(s"No project associated with ${target.uri}")
    }
  }

  protected def detectedJdkWithoutJDI(error: Throwable): String = {
    s"""Debugging is not supported because Java Debug Interface couldn't be resolved in detected JDK ${JavaRuntime.home}: '${error.getMessage}'. To enable it, check manually that you're running on a JDK and JDI is supported.
       |
       |Run $buildToolName about for more information about the current JDK runtime.""".stripMargin
  }

  protected def detectedUnsupportedJreForDebugging(error: Throwable): String = {
    s"""Debugging is not supported because $buildToolName is running on a JRE ${JavaRuntime.home} with no support for Java Debug Interface: '${error.getMessage}'. To enable debugging, install a JDK and restart $buildToolName.
       |
       |Run $buildToolName about for more information about the current JDK runtime.""".stripMargin
  }

  private def inferDebuggeeRunner(
    params: bsp.DebugSessionParams,
    projects: Seq[Project],
    state: State
  ): BspResponse[DebuggeeRunner] = {
    params.dataKind match {
      case bsp.DebugSessionParamsDataKind.ScalaMainClass =>
        convert[bsp.ScalaMainClass](params, state, main => runnerFactory.forMainClass(projects, state, main))
      case bsp.DebugSessionParamsDataKind.ScalaTestSuites =>
        convert[List[String]](params, state, filters => runnerFactory.forTestSuite(projects, state, filters))
      case bsp.DebugSessionParamsDataKind.ScalaAttachRemote =>
        convertError(runnerFactory.forAttachRemote(state))
      case dataKind => Left(JsonRpcResponse.invalidRequest(s"Unsupported data kind: $dataKind"))
    }
  }

  private def convert[A: Decoder](
    params: bsp.DebugSessionParams,
    state: State,
    f: A => Either[String, DebuggeeRunner]
  ): Either[ProtocolError, DebuggeeRunner] = {
    params.data.as[A] match {
      case Left(error) =>
        Left(JsonRpcResponse.invalidRequest(error.getMessage()))
      case Right(params) => convertError(f(params))
    }
  }

  private def convertError(runner: Either[String, DebuggeeRunner]): Either[ProtocolError, DebuggeeRunner] =
    runner match {
      case Right(adapter) => Right(adapter)
      case Left(error) => Left(JsonRpcResponse.invalidRequest(error))
    }

}