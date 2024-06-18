package loci
package communicator
package ws.akka

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object awaitBinding {
  def apply(listener: Listener[_], atMost: Duration): Boolean = {
    val binding = listener match {
      case listener: WSListener.BoundRoute[_] => listener.binding
      case _ => None
    }

    binding exists {
      Await.ready(_, atMost).value.get.isSuccess
    }
  }
}
