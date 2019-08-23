package loci
package registry

import transmitter.AbstractionRef
import transmitter.Channel
import transmitter.RemoteAccessException
import transmitter.RemoteRef
import scala.util.Try
import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.Promise

class Bindings[A <: AbstractionRef](
    request: (A, MessageBuffer) => Unit,
    respond: (A, MessageBuffer) => Unit) {

  private val bindings = new ConcurrentHashMap[
    String, (MessageBuffer, A) => Try[MessageBuffer]]

  private val responseHandlers = new ConcurrentHashMap[
    Channel, Promise[MessageBuffer]]

  def bind[T](binding: Binding[T])(function: RemoteRef => T): Unit =
    bindings put (binding.name, binding.dispatch(function, _, _))

  def lookup[T](binding: Binding[T], abstraction: A): binding.RemoteCall =
    binding.call(abstraction) { message =>
      val channel = abstraction.channel
      val promise = Promise[MessageBuffer]

      if (abstraction.remote.connected)
        responseHandlers put (channel, promise)
      else
        channelsClosed

      request(abstraction, message)
      promise.future
    }

  def processRequest(
      message: MessageBuffer, name: String, abstraction: A): Unit =
    Option(bindings get name) foreach { dispatch =>
      dispatch(message, abstraction) foreach { respond(abstraction, _) }
    }

  def processResponse(
      message: MessageBuffer, name: String, abstraction: A): Unit =
    Option(responseHandlers remove abstraction.channel) foreach { _ success message }

  def channelsClosed(): Unit = {
    val iterator = responseHandlers.entrySet.iterator
    while (iterator.hasNext) {
      val entry = iterator.next
      if (!entry.getKey.open) {
        entry.getValue tryFailure new RemoteAccessException(RemoteAccessException.RemoteDisconnected)
        iterator.remove
      }
    }
  }
}
