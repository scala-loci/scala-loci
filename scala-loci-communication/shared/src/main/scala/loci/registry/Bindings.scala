package loci
package registry

import transmitter.AbstractionRef
import transmitter.Channel
import transmitter.RemoteAccessException
import transmitter.RemoteRef

import scala.util.Failure
import scala.util.Try
import java.util.concurrent.ConcurrentHashMap

class Bindings[A <: AbstractionRef](
    request: (A, MessageBuffer) => Unit,
    respond: (A, Try[MessageBuffer]) => Unit) {

  private val bindings = new ConcurrentHashMap[
    String, (MessageBuffer, A) => Try[MessageBuffer]]

  private val responseHandlers = new ConcurrentHashMap[
    Channel, Notice.Steady.Source[Try[MessageBuffer]]]

  def bind[T](binding: Binding[T])(function: RemoteRef => T): Unit =
    bindings put (binding.name, binding.dispatch(function, _, _))

  def lookup[T](binding: Binding[T], abstraction: A): binding.RemoteCall =
    binding.call(abstraction) { message =>
      val channel = abstraction.channel
      val response = Notice.Steady[Try[MessageBuffer]]

      if (abstraction.remote.connected)
        responseHandlers put (channel, response)
      else
        channelsClosed

      request(abstraction, message)
      response.notice
    }

  def processRequest(
      message: MessageBuffer, name: String, abstraction: A): Unit =
    Option(bindings get name) foreach { dispatch =>
      respond(abstraction, dispatch(message, abstraction))
    }

  def processResponse(
      message: Try[MessageBuffer], name: String, abstraction: A): Unit =
    Option(responseHandlers remove abstraction.channel) foreach { _.set(message) }

  def channelsClosed(): Unit = {
    val iterator = responseHandlers.entrySet.iterator
    while (iterator.hasNext) {
      val entry = iterator.next
      if (!entry.getKey.open) {
        entry.getValue.trySet(Failure(new RemoteAccessException(RemoteAccessException.RemoteDisconnected)))
        iterator.remove
      }
    }
  }
}
