package loci
package transmitter
package dev

import java.util.concurrent.atomic.AtomicLong

trait ContextBuilder[T <: Transmittables, M <: Message.Transmittable] {
  def apply(abstraction: AbstractionRef): ContextBuilder.Context[T, M]
}

object ContextBuilder {
  sealed trait Context[T <: Transmittables, M <: Message.Transmittable]
    extends SendingContext[T, M] with ReceivingContext[T, M]

  implicit def message[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      S <: Transmittables](
    implicit
      contextBuilders: ContextBuilders[S],
      selector: Selector[B, I, R, T, M, S],
      serializerMessage: Serializable[I]) =
    new ContextBuilder[S, Transmittable.Aux[B, I, R, T, M]] {
      def apply(abstraction: AbstractionRef) =
        new Context[S, Transmittable.Aux[B, I, R, T, M]] {
          val contexts = contextBuilders(abstraction)

          val sendingTurn = new AtomicLong(0)
          val receivingTurn = new AtomicLong(0)

          def endpoint(implicit
              transmittable: Transmittable.Aux[B, I, R, T, M]) = {
            val contextBuilder = selector contextBuilder contextBuilders

            new Endpoint[transmittable.Base, transmittable.Result] {
              val receive = abstraction.channel.receive map
                (Function unlift { message =>
                  implicit val context = contextBuilder(
                    abstraction derive receivingTurn.getAndIncrement.toString)
                  (serializerMessage deserialize message).toOption map
                    transmittable.receive
                })

              def send(v: transmittable.Base) = {
                implicit val context = contextBuilder(
                  abstraction derive sendingTurn.getAndIncrement.toString)
                abstraction.channel send (
                  serializerMessage serialize (transmittable send v))
              }
            }
          }

          def send[B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              transmittable: Transmittable.Aux[B0, I0, R0, T0, M0], v: B0)(
            implicit
              selector: Selector[B0, I0, R0, T0, M0, S])
          : transmittable.Intermediate = {
            implicit val context = selector context contexts
            transmittable send v
          }

          def receive[B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              transmittable: Transmittable.Aux[B0, I0, R0, T0, M0], v: I0)(
            implicit
              selector: Selector[B0, I0, R0, T0, M0, S])
          : transmittable.Result = {
            implicit val context = selector context contexts
            transmittable receive v
          }
        }
    }

  implicit def nomessage[S <: Transmittables](
    implicit
      contextBuilders: ContextBuilders[S]) =
    new ContextBuilder[S, NoMessage] {
      def apply(abstraction: AbstractionRef) =
        new Context[S, NoMessage] {
          val contexts = contextBuilders(abstraction)

          def endpoint(implicit transmittable: NoMessage) =
            throw new TransmitterResolutionException(
              "MessageTransmittable",
              "Transmittable")

          def send[B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              transmittable: Transmittable.Aux[B0, I0, R0, T0, M0], v: B0)(
            implicit
              selector: Selector[B0, I0, R0, T0, M0, S])
          : transmittable.Intermediate = {
            implicit val context = selector context contexts
            transmittable send v
          }

          def receive[B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              transmittable: Transmittable.Aux[B0, I0, R0, T0, M0], v: I0)(
            implicit
              selector: Selector[B0, I0, R0, T0, M0, S])
          : transmittable.Result = {
            implicit val context = selector context contexts
            transmittable receive v
          }
        }
    }
}
