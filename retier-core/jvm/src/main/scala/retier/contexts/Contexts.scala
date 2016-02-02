package retier
package contexts

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory

object Pooled {
  lazy val global: ExecutionContextExecutor = ExecutionContext.global

  object Implicits {
    implicit lazy val global = Pooled.global
  }
}

object Immediate {
  lazy val global: ExecutionContextExecutor = new ExecutionContextExecutor {
    private[this] val report = ExecutionContext.defaultReporter

    def execute(runnable: Runnable) =
      try runnable.run
      catch { case NonFatal(exception) => reportFailure(exception) }

    def reportFailure(throwable: Throwable) = report(throwable)
  }

  object Implicits {
    implicit lazy val global = Immediate.global
  }
}

object Queued {
  lazy val global = create
  def create: ExecutionContextExecutor = ExecutionContext fromExecutorService
    (Executors newSingleThreadExecutor new ThreadFactory {
      def newThread(runnable: Runnable) = {
        val thread = new Thread(runnable)
        thread setDaemon true
        thread
      }
    })

  object Implicits {
    implicit lazy val global = Queued.global
  }
}
