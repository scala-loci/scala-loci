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
  sealed trait Context[T <: Transmittables]
    extends SendingContext[T] with ReceivingContext[T]

  sealed trait Equiv[+T0 <: Transmittables]

  final case class EndpointEquiv[B0, I0, R0, T0 <: Transmittables](
      endpoint: Endpoint[B0, R0])
    extends Equiv[Message[Transmittable.Aux[B0, I0, R0, T0]]]

  implicit def message[B, I, R, T <: Transmittables](implicit
      contextBuilders: ContextBuilders[Message[Transmittable.Aux[B, I, R, T]]],
      selector: Selector[B, I, R, T, Message[Transmittable.Aux[B, I, R, T]]],
      serializerMessage: Serializable[I])
  : ContextBuilder[Message[Transmittable.Aux[B, I, R, T]]] = {
    type M = Message[Transmittable.Aux[B, I, R, T]]

    new ContextBuilder[M] {
      def apply(transmittables: M, abstraction: AbstractionRef) =
        new Context[M] {
          val contexts = contextBuilders(transmittables, abstraction)

          val sendingTurn = new AtomicLong(0)
          val receivingTurn = new AtomicLong(0)

          def endpoint[B0, I0, R0, T0 <: Transmittables](implicit
              ev: Equiv[M] <:<
                  Equiv[Message[Transmittable.Aux[B0, I0, R0, T0]]]) = {
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

          def send[B0, I0, R0, T0 <: Transmittables](
              value: B0)(implicit selector: Selector[B0, I0, R0, T0, M]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) send value
          }

          def receive[B0, I0, R0, T0 <: Transmittables](
              value: I0)(implicit selector: Selector[B0, I0, R0, T0, M]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) receive value
          }
        }
    }
  }

  implicit def nomessage[S <: Delegating](implicit
      contextBuilders: ContextBuilders[Delegates[S]])
  : ContextBuilder[Delegates[S]] = {
    type D = Delegates[S]

    new ContextBuilder[Delegates[S]] {
      def apply(transmittables: D, abstraction: AbstractionRef) =
        new Context[D] {
          val contexts = contextBuilders(transmittables, abstraction)

          def endpoint[B0, I0, R0, T0 <: Transmittables](implicit
              ev: Equiv[D] <:<
                  Equiv[Message[Transmittable.Aux[B0, I0, R0, T0]]]) =
            throw new TransmitterResolutionException(
              "Equiv[Delegates[S]] <:< Equiv[Message[Transmittable[T]]]",
              "Message[Transmittable[T]]")

          def send[B0, I0, R0, T0 <: Transmittables](
              value: B0)(implicit selector: Selector[B0, I0, R0, T0, D]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) send value
          }

          def receive[B0, I0, R0, T0 <: Transmittables](
              value: I0)(implicit selector: Selector[B0, I0, R0, T0, D]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) receive value
          }
        }
    }
  }
}
