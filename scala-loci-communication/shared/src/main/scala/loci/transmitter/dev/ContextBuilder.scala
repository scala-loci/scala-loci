package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message, None }
import java.util.concurrent.atomic.AtomicLong

trait ContextBuilder[T <: Transmittables] {
  def apply(transmittables: T, abstraction: AbstractionRef)
    : ContextBuilder.Context[T]
}

object ContextBuilder {
  sealed abstract class Context[S <: Transmittables](
      transmittables: S,
      contexts: Contexts[S])
    extends SendingContext[S] with ReceivingContext[S] {

    def send[B, I, R, P, T <: Transmittables](
        value: B)(implicit selector: Selector[B, I, R, P, T, S]) = {
      implicit val context = selector context contexts
      (selector transmittable transmittables) send value
    }

    def receive[B, I, R, P, T <: Transmittables](
        value: I)(implicit selector: Selector[B, I, R, P, T, S]) = {
      implicit val context = selector context contexts
      (selector transmittable transmittables) receive value
    }
  }

  implicit def messaging[B, I, R, P, T <: Transmittables](implicit
      contextBuilder: ContextBuilder[T],
      serializer: Serializable[I])
  : ContextBuilder[Message[Transmittable.Aux[B, I, R, P, T]]] = {
    type M = Message[Transmittable.Aux[B, I, R, P, T]]

    new ContextBuilder[M] {
      def apply(transmittables: M, abstraction: AbstractionRef) = {
        val transmittable = transmittables.message
        val context = contextBuilder(transmittable.transmittables, abstraction)

        new Context[M](
            transmittables,
            new Contexts.SingleMessage(context)) with
          Context.MessageEndpoint[B, I, R, P, T] {

          val sendingTurn = new AtomicLong(0)
          val receivingTurn = new AtomicLong(0)

          val endpoint: Endpoint[B, R] = new Endpoint[B, R] {
            val receive = abstraction.channel.receive map
              (Function unlift { message =>
                implicit val context = contextBuilder(
                  transmittable.transmittables,
                  abstraction derive receivingTurn.getAndIncrement.toString)
                (serializer deserialize message).toOption map
                  transmittable.receive
              })

            def send(value: B) = {
              implicit val context = contextBuilder(
                transmittable.transmittables,
                abstraction derive sendingTurn.getAndIncrement.toString)
              abstraction.channel send (
                serializer serialize (transmittable send value))
            }
          }
        }
      }
    }
  }

  implicit def delegating[D <: Delegating](implicit
      contextBuilders: ContextBuilders[Delegates[D]])
  : ContextBuilder[Delegates[D]] =
    new ContextBuilder[Delegates[D]] {
      def apply(transmittables: Delegates[D], abstraction: AbstractionRef) =
        new Context[Delegates[D]](
            transmittables,
            contextBuilders(transmittables, abstraction)) with
          Context.DelegatesNoEndpoint[D]
    }

  implicit def none: ContextBuilder[None] =
    new ContextBuilder[None] {
      def apply(transmittables: None, abstraction: AbstractionRef) =
        new Context[None](transmittables, Contexts.None) with
          Context.NoneNoEndpoint
    }
}
