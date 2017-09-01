package loci
package transmitter
package dev

import java.util.concurrent.atomic.AtomicLong

trait ContextBuilder[T <: Transmittables, M <: Message.Transmittable] {
  def apply(transmittables: T, abstraction: AbstractionRef)
    : ContextBuilder.Context[T, M]
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
      def apply(transmittables: S, abstraction: AbstractionRef) =
        new Context[S, Transmittable.Aux[B, I, R, T, M]] {
          val contexts = contextBuilders(transmittables, abstraction)

          val sendingTurn = new AtomicLong(0)
          val receivingTurn = new AtomicLong(0)

          def endpoint(
              transmittable: Transmittable.Aux[B, I, R, T, M])(
            implicit
              ev: Transmittable.Aux[B, I, R, T, M] <:< Transmittable[_]) = {
            val contextBuilder = selector contextBuilder contextBuilders

            new Endpoint[transmittable.Base, transmittable.Result] {
              val receive = abstraction.channel.receive map
                (Function unlift { message =>
                  implicit val context = contextBuilder(
                    transmittable.transmittables,
                    abstraction derive receivingTurn.getAndIncrement.toString)
                  (serializerMessage deserialize message).toOption map
                    transmittable.receive
                })

              def send(value: transmittable.Base) = {
                implicit val context = contextBuilder(
                  transmittable.transmittables,
                  abstraction derive sendingTurn.getAndIncrement.toString)
                abstraction.channel send (
                  serializerMessage serialize (transmittable send value))
              }
            }
          }

          def send[
              B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              value: B0)(implicit selector: Selector[B0, I0, R0, T0, M0, S]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) send value
          }

          def receive[
              B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              value: I0)(implicit selector: Selector[B0, I0, R0, T0, M0, S]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) receive value
          }
        }
    }

  implicit def nomessage[S <: Transmittables](
    implicit
      contextBuilders: ContextBuilders[S]) =
    new ContextBuilder[S, NoMessage] {
      def apply(transmittables: S, abstraction: AbstractionRef) =
        new Context[S, NoMessage] {
          val contexts = contextBuilders(transmittables, abstraction)

          def endpoint(
              transmittable: NoMessage)(
            implicit
              ev: NoMessage <:< Transmittable[_]) =
            throw new TransmitterResolutionException(
              "M <:< Transmittable[_]",
              "Transmittable")

          def send[
              B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              value: B0)(implicit selector: Selector[B0, I0, R0, T0, M0, S]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) send value
          }

          def receive[
              B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
              value: I0)(implicit selector: Selector[B0, I0, R0, T0, M0, S]) = {
            implicit val context = selector context contexts
            (selector transmittable transmittables) receive value
          }
        }
    }
}
