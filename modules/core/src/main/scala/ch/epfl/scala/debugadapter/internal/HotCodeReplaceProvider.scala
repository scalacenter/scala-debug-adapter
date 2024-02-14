package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import com.microsoft.java.debug.core.DebugException
import com.microsoft.java.debug.core.DebugUtility
import com.microsoft.java.debug.core.IDebugSession
import com.microsoft.java.debug.core.StackFrameUtility
import com.microsoft.java.debug.core.adapter.HotCodeReplaceEvent
import com.microsoft.java.debug.core.adapter.IDebugAdapterContext
import com.microsoft.java.debug.core.adapter.IHotCodeReplaceProvider
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.microsoft.java.debug.core.adapter.IStackTraceProvider
import com.microsoft.java.debug.core.protocol.Events
import com.sun.jdi.*
import io.reactivex.Observable

import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicReference
import java.{util => ju}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal
import io.reactivex.subjects.PublishSubject
import java.io.Closeable

class HotCodeReplaceProvider(
    debuggee: Debuggee,
    logger: Logger,
    testMode: Boolean
) extends IHotCodeReplaceProvider {
  private var sourceLookUp: SourceLookUpProvider = null
  private var stackTraceProvider: StackTraceProvider = null
  private var subscription: Closeable = null
  private var context: IDebugAdapterContext = null
  private var currentDebugSession: IDebugSession = null
  private val classesAccumulator: AtomicReference[Set[String]] = new AtomicReference(Set.empty)
  private val threadFrameMap: mutable.Map[ThreadReference, Seq[StackFrame]] = mutable.Map.empty
  private val eventSubject = PublishSubject.create[HotCodeReplaceEvent]()

  override def initialize(context: IDebugAdapterContext, options: ju.Map[String, Object]): Unit = {
    this.context = context
    this.currentDebugSession = context.getDebugSession
    this.subscription = debuggee.observeClassesToUpdate(classes => classesAccumulator.updateAndGet(_ ++ classes))
    this.sourceLookUp = context.getProvider(classOf[ISourceLookUpProvider]).asInstanceOf[SourceLookUpProvider]
    this.stackTraceProvider = context.getProvider(classOf[IStackTraceProvider]).asInstanceOf[StackTraceProvider]
  }

  override def close(): Unit = {
    if (subscription != null) subscription.close()
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
      classesToReplace = classesAccumulator.getAndUpdate(_ => Set.empty).toSeq
      _ <- if (classesToReplace.isEmpty) Success(()) else doHotCodeReplace(classesToReplace)
    } yield {
      sourceLookUp.reload(classesToReplace)
      stackTraceProvider.reload()
      classesToReplace.filter(isClassLoaded).toList.asJava
    }
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

  override val getEventHub: Observable[HotCodeReplaceEvent] = eventSubject

  private def publishEvent(tpe: HotCodeReplaceEvent.EventType, message: String): Unit = {
    eventSubject.onNext(new HotCodeReplaceEvent(tpe, message));
  }

  private def publishEvent(tpe: HotCodeReplaceEvent.EventType, message: String, data: AnyRef): Unit = {
    eventSubject.onNext(new HotCodeReplaceEvent(tpe, message, data));
  }

  private def doHotCodeReplace(classesToReplace: Seq[String]): Try[Unit] = {
    if (!currentDebugSession.getVM().canRedefineClasses()) {
      val err = "JVM doesn't support hot reload classes"
      publishEvent(HotCodeReplaceEvent.EventType.ERROR, err)
      Failure(new DebugException(err))
    } else {
      val res = for {
        suspendedThreads <- getSuspendedThreads()
        poppedThreads <- canPopFrames.transform(
          _ => attemptPopFrames(suspendedThreads, classesToReplace),
          e => Try(warnOrThrow("Cannot pop frames")).map(_ => Seq.empty)
        )
        _ = publishEvent(HotCodeReplaceEvent.EventType.STARTING, "Starting hot code replacement...")
        _ = redefineClasses(classesToReplace)
        res <-
          if (containsObsoleteMethods(suspendedThreads)) {
            val err = "JVM contains obsolete methods"
            publishEvent(HotCodeReplaceEvent.EventType.ERROR, err);
            Failure(new DebugException(s"Failed to complete hot code replace: $err"))
          } else {
            poppedThreads.foreach(stepIntoThread)
            Success(())
          }
      } yield res
      publishEvent(HotCodeReplaceEvent.EventType.END, "Completed hot code replace", classesToReplace.asJava)
      threadFrameMap.clear()
      res
    }
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
      try
        if (oldValue.isEmpty || refresh) Some(thread.frames.asScala.toSeq)
        else oldValue
      catch {
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
          publishEvent(HotCodeReplaceEvent.EventType.ERROR, e.getMessage());
          Failure(new DebugException("Failed to redefine classes: " + e.getMessage()))
      }
  }

  private def getTypesToBytes(qualifiedNames: Seq[String]): Map[ReferenceType, Array[Byte]] =
    sourceLookUp match {
      case null => throw new IllegalAccessException("Source lookup is not available")
      case sourceLookUp =>
        {
          for {
            className <- qualifiedNames
            classFile <- sourceLookUp.getClassFile(className).toSeq
            bytes = classFile.readBytes()
            cls <- getJdiClassByName(className)
          } yield cls -> bytes
        }.toMap
    }

  private def getJdiClassByName(className: String): mutable.Buffer[ReferenceType] = {
    try currentDebugSession.getVM().classesByName(className).asScala
    catch {
      case NonFatal(e) =>
        warnOrThrow(s"Cannot get JDI class of $className", e)
        mutable.Buffer.empty
    }
  }

  private def containsObsoleteMethods(suspendedThreads: Seq[ThreadReference]): Boolean =
    suspendedThreads
      .flatMap(getStackFrames(_, true))
      .exists(StackFrameUtility.isObsolete)

  private def stepIntoThread(thread: ThreadReference): Unit = {
    val eventRequestManager = currentDebugSession.getVM().eventRequestManager();
    val request = DebugUtility.createStepIntoRequest(thread, context.getStepFilters.skipClasses)
    currentDebugSession.getEventHub.stepEvents
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

object HotCodeReplaceProvider {
  def apply(
      debuggee: Debuggee,
      logger: Logger,
      testMode: Boolean
  ): HotCodeReplaceProvider =
    new HotCodeReplaceProvider(debuggee, logger, testMode)
}
