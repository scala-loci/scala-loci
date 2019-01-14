package loci
package registry

import transmitter.AbstractionRef
import transmitter.Channel
import transmitter.RemoteRef
import scala.util.Try
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue

class Bindings[A <: AbstractionRef](
    request: (A, MessageBuffer) => Unit,
    respond: (A, MessageBuffer) => Unit) {

  private val bindings = new ConcurrentHashMap[
    String, (MessageBuffer, A) => Try[MessageBuffer]]

  private val responseHandlers = new ConcurrentHashMap[
    Channel, ConcurrentLinkedQueue[MessageBuffer => Unit]]

  def bind[T](binding: Binding[T])(function: RemoteRef => T): Unit =
    bindings put (binding.name, binding.dispatch(function, _, _))

  def lookup[T](binding: Binding[T], abstraction: A): binding.RemoteCall =
    binding.call(abstraction) { message => handler =>
      val channel = abstraction.channel
      val handlers = Option(responseHandlers get channel) getOrElse {
        val handlers = new ConcurrentLinkedQueue[MessageBuffer => Unit]
        (Option(responseHandlers putIfAbsent (channel, handlers))
          getOrElse handlers)
      }

      if (abstraction.remote.connected)
        handlers add handler
      else
        channelsClosed

      request(abstraction, message)
    }

  def processRequest(
      message: MessageBuffer, name: String, abstraction: A): Unit =
    Option(bindings get name) foreach { dispatch =>
      dispatch(message, abstraction) foreach { respond(abstraction, _) }
    }

  def processResponse(
      message: MessageBuffer, name: String, abstraction: A): Unit =
    Option(responseHandlers get abstraction.channel) foreach { handlers =>
      Option(handlers.poll) foreach { _(message) }
    }

  def channelsClosed(): Unit = {
    val iterator = responseHandlers.keySet.iterator
    while (iterator.hasNext)
      if (!iterator.next.open)
        iterator.remove
  }
}
