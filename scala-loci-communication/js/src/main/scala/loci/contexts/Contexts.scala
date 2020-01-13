package loci
package contexts

import scala.util.control.NonFatal
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.scalajs.concurrent.JSExecutionContext

object Pooled {
  lazy val global: ExecutionContextExecutor =
    new logging.ReportingExecutionContext(JSExecutionContext.queue)

  object Implicits {
    implicit lazy val global: ExecutionContext = Pooled.global
  }
}

object Immediate {
  lazy val global: ExecutionContextExecutor = new ExecutionContextExecutor {
    def execute(runnable: Runnable) =
      try runnable.run()
      catch { case NonFatal(exception) => reportFailure(exception) }

    def reportFailure(throwable: Throwable) = logging.reportException(throwable)
  }

  object Implicits {
    implicit lazy val global: ExecutionContext = Immediate.global
  }
}

object Queued {
  lazy val global = create
  def create: ExecutionContextExecutor =
    new logging.ReportingExecutionContext(JSExecutionContext.queue)

  object Implicits {
    implicit lazy val global: ExecutionContext = Queued.global
  }
}
