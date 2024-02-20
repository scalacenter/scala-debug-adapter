package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import com.microsoft.java.debug.core.DebugException
import com.microsoft.java.debug.core.DebugUtility
import com.microsoft.java.debug.core.StackFrameUtility
import com.microsoft.java.debug.core.adapter.IDebugAdapterContext
import com.microsoft.java.debug.core.protocol.Events
import com.sun.jdi.*

import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

private[internal] class HotCodeReplacer(
    sourceLookUp: SourceLookUpProvider,
    context: IDebugAdapterContext,
    logger: Logger,
    testMode: Boolean
) {
  def canRedefineClassesAndPopFrames: Try[Unit] = {
    if (!vm.canRedefineClasses)
      Failure(new DebugException("JVM does not support hot reload classes"))
    else if (!vm.canPopFrames)
      Failure(new DebugException("JVM does not support popping frames"))
    else Success(())
  }

  def replace(classNames: Seq[String]): Try[Seq[String]] = {
    val loadedClasses = classNames.flatMap(getLoadedClass)
    for {
      suspendedThreads <- getSuspendedThreads()
      poppedThreads <- attemptPopFrames(suspendedThreads, loadedClasses)
      _ <- redefineClasses(loadedClasses)
      _ <-
        if (containsObsoleteMethods(suspendedThreads)) fail("JVM contains obsolete method")
        else {
          poppedThreads.foreach(stepIntoThread)
          Success(())
        }
    } yield loadedClasses.map(_.name).distinct
  }

  private def vm: VirtualMachine = context.getDebugSession.getVM

  private def fail(message: String): Failure[Nothing] =
    Failure(new DebugException(s"Failed to perform hot code replace: $message"))

  private def warnOrThrow(message: String) =
    if (testMode) throw new DebugException(message)
    else logger.warn(message)

  private def warnOrThrow(message: String, e: Throwable) =
    if (testMode) throw e
    else logger.warn(s"$message because of ${e.getClass.getSimpleName}: ${e.getMessage}")

  private def attemptPopFrames(
      threads: Seq[ThreadReference],
      classes: Seq[ReferenceType]
  ): Try[Seq[ThreadReference]] = {
    val poppedThreads =
      for {
        thread <- threads
        stack <- getStackSafely(thread)
        // we cannot drop the last frame, i.e. the main method
        (frameToPop, _) <- stack.zipWithIndex.reverse.tail.find { case (frame, i) =>
          containsChangedType(frame, classes) && !stack.iterator.take(i + 1).exists(StackFrameUtility.isNative)
        }
      } yield Try {
        StackFrameUtility.pop(frameToPop)
        thread
      }
    // if there is one frame that we fail to pop, we abort the hot code replacement
    poppedThreads.traverse
  }

  private def containsChangedType(frame: StackFrame, classes: Seq[ReferenceType]): Boolean = {
    val frameDeclaringType = StackFrameUtility.getDeclaringType(frame).name
    // Check if the frame's declaring type has changed or if one of its inner classes have changed
    classes.exists(_.name.startsWith(frameDeclaringType))
  }

  private def getStackSafely(thread: ThreadReference): Option[Seq[StackFrame]] = {
    try Some(thread.frames.asScala.toSeq)
    catch {
      case e: IncompatibleThreadStateException =>
        warnOrThrow(s"Failed to get stack frames of ${thread.name}: ${e.getMessage}")
        None
    }
  }

  private def getSuspendedThreads(): Try[Seq[ThreadReference]] =
    Try(vm.allThreads.asScala.toSeq.filter(_.isSuspended))

  private def redefineClasses(classes: Seq[ReferenceType]): Try[Unit] = Try {
    val classToBytes = for {
      cls <- classes
      classFile <- sourceLookUp.getClassFile(cls.name)
    } yield cls -> classFile.readBytes()
    vm.redefineClasses(classToBytes.toMap.asJava)
  }

  private def getLoadedClass(className: String): Seq[ReferenceType] = {
    try vm.classesByName(className).asScala.toSeq
    catch {
      case NonFatal(e) =>
        warnOrThrow(s"Cannot get JDI class of $className", e)
        Seq.empty
    }
  }

  private def containsObsoleteMethods(suspendedThreads: Seq[ThreadReference]): Boolean =
    suspendedThreads
      .flatMap(getStackSafely(_).getOrElse(Seq.empty))
      .exists(StackFrameUtility.isObsolete)

  private def stepIntoThread(thread: ThreadReference): Unit = {
    val eventRequestManager = vm.eventRequestManager();
    val request = DebugUtility.createStepIntoRequest(thread, context.getStepFilters.skipClasses)
    context.getDebugSession.getEventHub.stepEvents
      .filter(debugEvent => request == debugEvent.event.request)
      .take(1)
      .subscribe { debugEvent =>
        debugEvent.shouldResume = false
        DebugUtility.deleteEventRequestSafely(eventRequestManager, request)
        context.getProtocolServer.sendEvent(new Events.StoppedEvent("step", thread.uniqueID()))
        // TODO do we need to send this event?
        // context.getProtocolServer.sendEvent(new Events.ContinuedEvent(thread.uniqueID()))
      }
    request.enable()
    thread.resume()
  }
}
