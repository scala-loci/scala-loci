package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message }
import java.util.concurrent.atomic.AtomicLong

trait ContextBuilder[T <: Transmittables] {
  def apply(transmittables: T, abstraction: AbstractionRef)
    : ContextBuilder.Context[T]
}

object ContextBuilder {
  sealed trait Equiv[+T <: Transmittables]

  final case class EndpointEquiv[B, I, R, P, T <: Transmittables](
      endpoint: Endpoint[B, R])
    extends Equiv[Message[Transmittable.Aux[B, I, R, P, T]]]


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
      contextBuilders: ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]],
      selector: Selector[B, I, R, P, T, Message[Transmittable.Aux[B, I, R, P, T]]],
      serializerMessage: Serializable[I])
  : ContextBuilder[Message[Transmittable.Aux[B, I, R, P, T]]] = {
    type M = Message[Transmittable.Aux[B, I, R, P, T]]

    new ContextBuilder[M] {
      def apply(transmittables: M, abstraction: AbstractionRef) =
        new Context[M](
            transmittables,
            contextBuilders(transmittables, abstraction)) {
          val sendingTurn = new AtomicLong(0)
          val receivingTurn = new AtomicLong(0)

          def endpoint[B0, I0, R0, C0, T0 <: Transmittables](implicit
              ev: Equiv[M] <:<
                  Equiv[Message[Transmittable.Aux[B0, I0, R0, C0, T0]]]) = {
            val contextBuilder = selector contextBuilder contextBuilders
            val transmittable = transmittables.message

            val EndpointEquiv(endpoint) = ev(EndpointEquiv(new Endpoint[B, R] {
              val receive = abstraction.channel.receive map
                (Function unlift { message =>
                  implicit val context = contextBuilder(
                    transmittable.transmittables,
                    abstraction derive receivingTurn.getAndIncrement.toString)
                  (serializerMessage deserialize message).toOption map
                    transmittable.receive
                })

              def send(value: B) = {
                implicit val context = contextBuilder(
                  transmittable.transmittables,
                  abstraction derive sendingTurn.getAndIncrement.toString)
                abstraction.channel send (
                  serializerMessage serialize (transmittable send value))
              }
            }))

            endpoint
          }
        }
    }
  }

  implicit def delegating[S <: Delegating](implicit
      contextBuilders: ContextBuilders[Delegates[S]])
  : ContextBuilder[Delegates[S]] = {
    type D = Delegates[S]

    new ContextBuilder[Delegates[S]] {
      def apply(transmittables: D, abstraction: AbstractionRef) =
        new Context[D](
            transmittables,
            contextBuilders(transmittables, abstraction)) {
          def endpoint[B, I, R, P, T <: Transmittables](implicit
            ev: Equiv[D] <:< Equiv[Message[Transmittable.Aux[B, I, R, P, T]]]) = ???
        }
    }
  }
}
