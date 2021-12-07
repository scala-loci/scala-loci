package loci
package communicator
package ws.akka

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object awaitBinding {
  def apply(listener: Listener[WS], atMost: Duration): Unit = {
    val binding = listener match {
      case listener: WSListener.BoundRoute[WS] => listener.binding
      case _ => None
    }

    binding.fold(throw new NoSuchElementException("listener has no binding")) {
      Await.ready(_, atMost)
    }
  }
}
