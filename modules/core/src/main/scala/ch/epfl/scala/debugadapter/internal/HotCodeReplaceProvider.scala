package ch.epfl.scala.debugadapter.internal

import ch.epfl.scala.debugadapter.Debuggee
import ch.epfl.scala.debugadapter.Logger
import ch.epfl.scala.debugadapter.internal.ScalaExtension.*
import com.microsoft.java.debug.core.DebugException
import com.microsoft.java.debug.core.adapter.HotCodeReplaceEvent
import com.microsoft.java.debug.core.adapter.IDebugAdapterContext
import com.microsoft.java.debug.core.adapter.IHotCodeReplaceProvider
import com.microsoft.java.debug.core.adapter.ISourceLookUpProvider
import com.microsoft.java.debug.core.adapter.IStackTraceProvider
import io.reactivex.Observable
import io.reactivex.subjects.PublishSubject

import java.io.Closeable
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicReference
import java.{util => ju}
import scala.jdk.CollectionConverters.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try

class HotCodeReplaceProvider(
    debuggee: Debuggee,
    logger: Logger,
    testMode: Boolean
) extends IHotCodeReplaceProvider {
  private var sourceLookUp: SourceLookUpProvider = null
  private var stackTraceProvider: StackTraceProvider = null
  private var replacer: HotCodeReplacer = null
  private var subscription: Closeable = null
  private val classesAccumulator: AtomicReference[Set[String]] = new AtomicReference(Set.empty)
  private val eventSubject = PublishSubject.create[HotCodeReplaceEvent]()

  override def initialize(context: IDebugAdapterContext, options: ju.Map[String, Object]): Unit = {
    this.subscription = debuggee.observeClassUpdates(classes => classesAccumulator.updateAndGet(_ ++ classes))
    this.sourceLookUp = context.getProvider(classOf[ISourceLookUpProvider]).asInstanceOf[SourceLookUpProvider]
    this.stackTraceProvider = context.getProvider(classOf[IStackTraceProvider]).asInstanceOf[StackTraceProvider]
    this.replacer = new HotCodeReplacer(sourceLookUp, context, logger, testMode)
  }

  override def close(): Unit = {
    if (subscription != null) subscription.close()
    if (eventSubject.hasComplete()) eventSubject.onComplete()
    sourceLookUp = null
    stackTraceProvider = null
    replacer = null
    subscription = null
    classesAccumulator.set(null)
  }

  override def onClassRedefined(consumer: ju.function.Consumer[ju.List[String]]): Unit = ???

  override val getEventHub: Observable[HotCodeReplaceEvent] = eventSubject

  override def redefineClasses(): CompletableFuture[ju.List[String]] = {
    publishEvent(HotCodeReplaceEvent.EventType.STARTING, "Starting hot code replacement...")
    val res = for {
      _ <- initialized
      _ <- replacer.canRedefineClassesAndPopFrames
      classNames = classesAccumulator.getAndUpdate(_ => Set.empty).toSeq
      replacedClasses <- if (classNames.isEmpty) Success(Seq.empty) else replacer.replace(classNames)
    } yield {
      sourceLookUp.reload(classNames)
      stackTraceProvider.reload()
      replacedClasses.asJava
    }
    res match {
      case Failure(exception) =>
        val message = s"Failed hot code replacement because of ${exception.getClass.getName}: ${exception.getMessage}"
        publishEvent(HotCodeReplaceEvent.EventType.ERROR, message)
      case Success(replacedClasses) =>
        publishEvent(HotCodeReplaceEvent.EventType.END, "Completed hot code replacement", replacedClasses)
    }
    res.toCompletableFuture
  }

  private def initialized: Try[Unit] =
    if (replacer == null) Failure(new DebugException("HotCodeReplacer not initialized"))
    else Success(())

  private def publishEvent(tpe: HotCodeReplaceEvent.EventType, message: String): Unit =
    eventSubject.onNext(new HotCodeReplaceEvent(tpe, message));

  private def publishEvent(tpe: HotCodeReplaceEvent.EventType, message: String, data: AnyRef): Unit =
    eventSubject.onNext(new HotCodeReplaceEvent(tpe, message, data));
}

object HotCodeReplaceProvider {
  def apply(
      debuggee: Debuggee,
      logger: Logger,
      testMode: Boolean
  ): HotCodeReplaceProvider =
    new HotCodeReplaceProvider(debuggee, logger, testMode)
}
