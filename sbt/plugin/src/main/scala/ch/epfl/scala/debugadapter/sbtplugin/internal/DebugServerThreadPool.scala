package ch.epfl.scala.debugadapter.sbtplugin.internal

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ThreadPoolExecutor

private[debugadapter] object DebugServerThreadPool {
  private val nextThreadId = new AtomicInteger(1)
  private val threadGroup = Thread.currentThread.getThreadGroup

  private val threadFactory = new java.util.concurrent.ThreadFactory() {
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(
        threadGroup,
        runnable,
        s"debug-adapter-thread-${nextThreadId.getAndIncrement}"
      )
      thread.setDaemon(true)
      thread
    }
  }

  val executor = new ThreadPoolExecutor(
    1, /* corePoolSize */
    4, /* maxPoolSize */
    2,
    java.util.concurrent.TimeUnit.SECONDS, // keep alive 2 seconds
    new java.util.concurrent.SynchronousQueue[Runnable](),
    threadFactory
  )
}
