package loci
package communicator
package ws.akka

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object awaitTermination {
  def apply(atMost: Duration): Unit =
    Await.ready(WSActorSystem.terminated, atMost)
}
