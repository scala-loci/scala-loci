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
      T <: Transmittables,
      MB, MI, MR, MT <: Transmittables, MM <: Message.Transmittable](
    implicit
      contextBuilders: ContextBuilders[T],
      selector: Selector[Transmittable.Aux[MB, MI, MR, MT, MM], T],
      serializerMessage: Serializable[MI]) =
    new ContextBuilder[T, Transmittable.Aux[MB, MI, MR, MT, MM]] {
      def apply(abstraction: AbstractionRef) =
        new Context[T, Transmittable.Aux[MB, MI, MR, MT, MM]] {
          val contexts = contextBuilders(abstraction)

          val sendingTurn = new AtomicLong(0)
          val receivingTurn = new AtomicLong(0)

          def endpoint(implicit
              transmittable: Transmittable.Aux[MB, MI, MR, MT, MM]) = {
            val contextBuilder =
              selector contextBuilder (transmittable, contextBuilders)

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

          def send[U, V](transmittable: Transmittable[U], v: V)(
            implicit
              ev: V <:< transmittable.Base,
              selector: Selector[transmittable.Type, T])
          : transmittable.Intermediate = {
            implicit val context = selector context (transmittable, contexts)
            transmittable send v
          }

          def receive[U, V](transmittable: Transmittable[U], v: V)(
            implicit
              ev: V <:< transmittable.Intermediate,
              selector: Selector[transmittable.Type, T])
          : transmittable.Result = {
            implicit val context = selector context (transmittable, contexts)
            transmittable receive v
          }
        }
    }

  implicit def nomessage[T <: Transmittables](
    implicit
      contextBuilders: ContextBuilders[T]) =
    new ContextBuilder[T, NoMessage] {
      def apply(abstraction: AbstractionRef) =
        new Context[T, NoMessage] {
          val contexts = contextBuilders(abstraction)

          def endpoint(implicit transmittable: NoMessage) =
            throw new TransmitterResolutionException(
              "MessageTransmittable",
              "Transmittable")

          def send[U, V](transmittable: Transmittable[U], v: V)(
            implicit
              ev: V <:< transmittable.Base,
              selector: Selector[transmittable.Type, T])
          : transmittable.Intermediate = {
            implicit val context = selector context (transmittable, contexts)
            transmittable send v
          }

          def receive[U, V](transmittable: Transmittable[U], v: V)(
            implicit
              ev: V <:< transmittable.Intermediate,
              selector: Selector[transmittable.Type, T])
          : transmittable.Result = {
            implicit val context = selector context (transmittable, contexts)
            transmittable receive v
          }
        }
    }
}
