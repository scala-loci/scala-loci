package retier

import scala.concurrent.Future
import scala.concurrent.Awaitable

trait Runtime extends Awaitable[Unit] {
  def terminate(): Unit
  val terminated: Future[Unit]
}
