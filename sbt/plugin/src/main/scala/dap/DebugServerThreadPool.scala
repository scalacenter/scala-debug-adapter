package dap

object DebugServerThreadPool {
  private val nextThreadId = new java.util.concurrent.atomic.AtomicInteger(1)
  private val threadGroup = Thread.currentThread.getThreadGroup()

  private val threadFactory = new java.util.concurrent.ThreadFactory() {
    override def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(
        threadGroup,
        runnable,
        s"debug-server-thread-${nextThreadId.getAndIncrement}"
      )
      thread.setDaemon(true)
      thread
    }
  }

  private[dap] val executor = new java.util.concurrent.ThreadPoolExecutor(
    0, /* corePoolSize */
    12, /* maxPoolSize */
    2, java.util.concurrent.TimeUnit.SECONDS, // keep alive 2 seconds
    new java.util.concurrent.SynchronousQueue[Runnable](),
    threadFactory
  )
}
