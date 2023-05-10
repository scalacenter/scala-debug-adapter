package ch.epfl.scala.debugadapter.internal

import com.microsoft.java.debug.core.adapter.HotCodeReplaceEvent
import com.microsoft.java.debug.core.adapter.IHotCodeReplaceProvider
import io.reactivex.Observable

import java.util
import java.util.concurrent.CompletableFuture
import java.util.function.Consumer

object HotCodeReplaceProvider extends IHotCodeReplaceProvider {
  override def onClassRedefined(consumer: Consumer[util.List[String]]): Unit =
    ()

  override def redefineClasses(): CompletableFuture[util.List[String]] =
    CompletableFuture.completedFuture(util.Collections.emptyList())

  override def getEventHub: Observable[HotCodeReplaceEvent] =
    Observable.empty()
}
