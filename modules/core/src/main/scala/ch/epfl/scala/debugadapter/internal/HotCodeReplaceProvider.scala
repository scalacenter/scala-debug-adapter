package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.IHotCodeReplaceProvider
import io.reactivex.Observable
import java.{util => ju}
import java.util.concurrent.CompletableFuture
import java.util.Collections
import java.util.concurrent.atomic.AtomicReference
import io.reactivex.disposables.Disposable
import com.microsoft.java.debug.core.adapter.HotCodeReplaceEvent
import com.microsoft.java.debug.core.adapter.IDebugAdapterContext
import io.reactivex.functions.Consumer
import com.microsoft.java.debug.core.IDebugSession
import com.sun.jdi.*
import com.microsoft.java.debug.core.DebugException
import ch.epfl.scala.debugadapter.Logger
import scala.collection.JavaConverters.*
import com.microsoft.java.debug.core.StackFrameUtility
import scala.collection.mutable
import com.microsoft.java.debug.core.DebugUtility
import com.microsoft.java.debug.core.protocol.Events
import scala.util.Success
import scala.util.Failure
import scala.util.Try
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import scala.util.control.NonFatal

class HotCodeReplaceProvider(
    sourceLookup: SourceLookUpProvider,
    classesObs: Observable[Seq[String]],
    logger: Logger,
    testMode: Boolean
) extends IHotCodeReplaceProvider {
  private var subscription: Disposable = null
  private var context: IDebugAdapterContext = null
  private var currentDebugSession: IDebugSession = null
  private val classesAccumulator: AtomicReference[Set[String]] = new AtomicReference(Set.empty)
  private val threadFrameMap: mutable.Map[ThreadReference, Seq[StackFrame]] = mutable.Map.empty

  override def initialize(context: IDebugAdapterContext, options: ju.Map[String, Object]): Unit = {
    this.context = context
    this.currentDebugSession = context.getDebugSession
    this.subscription = classesObs.subscribe(classes => classesAccumulator.updateAndGet(_ ++ classes))
  }

  override def close(): Unit = {
    if (subscription != null) subscription.dispose()
    context = null
    currentDebugSession = null
    subscription = null
  }

  override def onClassRedefined(consumer: ju.function.Consumer[ju.List[String]]): Unit = ???

  override def redefineClasses(): CompletableFuture[ju.List[String]] = {
    def isClassLoaded(className: String): Boolean =
      getJdiClassByName(className).nonEmpty
    val res = for {
      _ <- initialized
      _ <- canRedefineClasses
      classesToReplace = classesAccumulator.getAndUpdate(_ => Set.empty).toSeq.filter(isClassLoaded)
      _ <- if (classesToReplace.isEmpty) Success(()) else doHotCodeReplace(classesToReplace)
    } yield classesToReplace.toList.asJava
    res.toCompletableFuture
  }

  private def initialized: Try[Unit] =
    if (currentDebugSession == null)
      Failure(new DebugException("HotCodeReplaceProvider not initialized"))
    else Success(())

  private def canRedefineClasses: Try[Unit] =
    if (!currentDebugSession.getVM.canRedefineClasses)
      Failure(new DebugException("JVM does not support hot reload classes"))
    else Success(())

  private def canPopFrames: Try[Unit] =
    if (!currentDebugSession.getVM.canPopFrames)
      Failure(new DebugException("JVM does not support popping frames"))
    else Success(())

  override def getEventHub: Observable[HotCodeReplaceEvent] =
    Observable.empty()

  private def doHotCodeReplace(classesToReplace: Seq[String]): Try[Unit] = {
    val res = for {
      suspendedThreads <- getSuspendedThreads()
      poppedThreads <- canPopFrames.transform(
        _ => attemptPopFrames(suspendedThreads, classesToReplace),
        e => Try(warnOrThrow("Cannot pop frames")).map(_ => Seq.empty)
      )
      _ = redefineClasses(classesToReplace)
      res <-
        if (containsObsoleteMethods(suspendedThreads))
          Failure(new DebugException("Failed to complete hot code replace: JVM still contains obsolete code"))
        else {
          poppedThreads.foreach(stepIntoThread)
          Success(())
        }
    } yield res
    threadFrameMap.clear()
    res
  }

  private def warnOrThrow(message: String) =
    if (testMode) throw new DebugException(message)
    else logger.warn(message)

  private def warnOrThrow(message: String, e: Throwable) =
    if (testMode) throw e
    else logger.warn(s"$message because of ${e.getClass.getSimpleName}: ${e.getMessage}")

  /**
   * Looks for the deepest affected stack frames in the stack and forces pop
   * affected frames. Does this for all of the active stack frames in the session.
   */
  private def attemptPopFrames(
      suspendedThreads: Seq[ThreadReference],
      classNames: Seq[String]
  ): Try[Seq[ThreadReference]] = {
    val poppedThreads =
      for {
        thread <- suspendedThreads
        frame <- getAffectedFrame(thread, classNames)
      } yield popStackFrame(thread, frame).map(_ => thread)
    poppedThreads.traverse
  }

  /**
   * Returns the stack frame that should be dropped to in the given thread after a
   * hot code replace. This is calculated by determining if the threads contain
   * stack frames that reside in one of the given replaced class names. If
   * possible, only stack frames whose methods were directly affected (and not
   * simply all frames in affected types) will be returned.
   */
  private def getAffectedFrame(thread: ThreadReference, qualifiedNames: Seq[String]): Option[StackFrame] = {
    val frames = getStackFrames(thread, false)
    val affectedFrame = frames.zipWithIndex
      .find { case (frame, _) => containsChangedType(frame, qualifiedNames) }
    affectedFrame.flatMap {
      case (frame, i) if !supportsDropToFrame(thread, frame) =>
        // The affected frame cannot be dropped.
        // Set the affected frame to the next lowest pop-able
        // frame on the stack.
        i.to(0, -1).map(frames.apply).find(supportsDropToFrame(thread, _))
      case (frame, _) => Some(frame)
    }
  }

  private def containsChangedType(frame: StackFrame, qualifiedNames: Seq[String]): Boolean = {
    val declaringTypeName: String = StackFrameUtility.getDeclaringType(frame).name
    // Check if the frame's declaring type have changed
    // or if one of its inner classes have changed
    qualifiedNames.exists { className =>
      className.startsWith(declaringTypeName)
    }
  }

  private def getStackFrames(thread: ThreadReference, refresh: Boolean): Seq[StackFrame] = {
    val oldValue = threadFrameMap.get(thread)
    val newValue =
      try {
        if (oldValue.isEmpty || refresh) Some(thread.frames.asScala.toSeq)
        else oldValue
      } catch {
        case e: IncompatibleThreadStateException =>
          logger.error(s"Failed to get stack frames: ${e.getMessage}")
          oldValue
      }
    newValue.foreach(threadFrameMap.update(thread, _))
    newValue.getOrElse(Seq.empty)
  }

  private def supportsDropToFrame(thread: ThreadReference, frame: StackFrame): Boolean =
    getStackFrames(thread, false).takeWhile(_ != frame).forall(StackFrameUtility.isNative)

  private def popStackFrame(thread: ThreadReference, frame: StackFrame): Try[Unit] = {
    val res = Try {
      // TODO rewrite recursive style
      var frames = getStackFrames(thread, false)
      val desiredSize = frames.indexOf(frame)
      0.to(frames.indexOf(frame)).foreach { _ =>
        StackFrameUtility.pop(frames.head)
        frames = getStackFrames(thread, true)
      }
    }
    res.recover { case e: DebugException =>
      warnOrThrow(s"Cannot pop frame of ${thread.name}", e)
    }
  }

  private def getSuspendedThreads(): Try[Seq[ThreadReference]] =
    Try(currentDebugSession.getVM.allThreads.asScala.toSeq.filter(_.isSuspended))

  private def redefineClasses(qualifiedNames: Seq[String]): Try[Unit] = {
    val typesToBytes = getTypesToBytes(qualifiedNames).asJava
    Try(currentDebugSession.getVM.redefineClasses(typesToBytes))
      .recoverWith {
        case e @ (
              _: UnsupportedOperationException | _: NoClassDefFoundError | _: VerifyError | _: ClassFormatError |
              _: ClassCircularityError
            ) =>
          // publishEvent(HotCodeReplaceEvent.EventType.ERROR, e.getMessage());
          Failure(new DebugException("Failed to redefine classes: " + e.getMessage()))
      }
  }

  private def getTypesToBytes(qualifiedNames: Seq[String]): Map[ReferenceType, Array[Byte]] = {
    qualifiedNames.flatMap { className =>
      for {
        classFile <- sourceLookup.getClassFile(className).toSeq
        bytes = classFile.readBytes()
        cls <- getJdiClassByName(className)
      } yield cls -> bytes
    }.toMap
  }

  private def getJdiClassByName(className: String): Seq[ReferenceType] = {
    try currentDebugSession.getVM().classesByName(className).asScala.toSeq
    catch {
      case NonFatal(e) =>
        warnOrThrow(s"Cannot get JDI class of $className", e)
        Seq.empty
    }
  }

  private def containsObsoleteMethods(suspendedThreads: Seq[ThreadReference]): Boolean =
    suspendedThreads
      .flatMap(getStackFrames(_, true))
      .exists(StackFrameUtility.isObsolete)

  private def stepIntoThread(thread: ThreadReference): Unit = {
    val request = DebugUtility.createStepIntoRequest(thread, context.getStepFilters.classNameFilters)
    currentDebugSession.getEventHub.stepEvents
      .filter(debugEvent => request.equals(debugEvent.event.request))
      .take(1)
      .subscribe { debugEvent =>
        debugEvent.shouldResume = false
        // Have to send to events to keep the UI sync with the step in operations:
        context.getProtocolServer.sendEvent(new Events.StoppedEvent("step", thread.uniqueID()))
        context.getProtocolServer.sendEvent(new Events.ContinuedEvent(thread.uniqueID()))
      }
    request.enable()
    thread.resume()
  }
}

object HotCodeReplaceProvider {
  def apply(
      sourceLookUp: SourceLookUpProvider,
      classesToUpdate: Observable[Seq[String]],
      logger: Logger,
      testMode: Boolean
  ): IHotCodeReplaceProvider =
    new HotCodeReplaceProvider(sourceLookUp, classesToUpdate, logger, testMode)
}
