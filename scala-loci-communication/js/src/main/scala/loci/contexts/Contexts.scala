package loci
package contexts

import scala.util.control.NonFatal
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor

object Pooled {
  lazy val global = scala.scalajs.concurrent.JSExecutionContext.queue

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
  def create = scala.scalajs.concurrent.JSExecutionContext.queue

  object Implicits {
    implicit lazy val global = Queued.global
  }
}
