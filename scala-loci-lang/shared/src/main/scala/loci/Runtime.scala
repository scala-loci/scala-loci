package loci

import scala.concurrent.{Awaitable, Future}

abstract class Runtime[P] private[loci] extends Awaitable[Unit] {
  val started: Notification[Instance[P]]
  val instance: Future[Instance[P]]
  def instances: Seq[Instance[P]]

  def terminate(): Unit
  val terminated: Future[Unit]
}
