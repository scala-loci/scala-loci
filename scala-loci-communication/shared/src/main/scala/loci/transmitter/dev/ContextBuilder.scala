package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message, None }
import java.util.concurrent.atomic.AtomicLong

trait ContextBuilder[T <: Transmittables] {
  def apply(
     transmittables: T,  abstraction: AbstractionRef,
     direction: ContextBuilder.Direction, index: Long = 0l)
  : ContextBuilder.Context[T]
}

object ContextBuilder {
  sealed class Direction private[ContextBuilder]
  val sending = new Direction
  val receiving = new Direction

  sealed abstract class Context[S <: Transmittables](
      val transmittables: S,
      val index: Long,
      val contexts: Contexts[S])
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
      def apply(
          transmittables: M, abstraction: AbstractionRef,
          direction: Direction, index: Long) = {
        val messagingAbstraction = abstraction derive index.toString
        val transmittable = transmittables.message
        val context = contextBuilder(
          transmittable.transmittables,
          messagingAbstraction derive "~0",
          direction)

        new Context[M](
            transmittables,
            index + 1l,
            new Contexts.SingleMessage(context, index)) with
          Context.MessageEndpoint[B, I, R, P, T] {

          val sendingTurn = new AtomicLong(1)
          val receivingTurn = new AtomicLong(1)

          def serialize(value: B) = {
            val turn = sendingTurn.getAndIncrement
            val directedTurn = if (direction == sending) s"+$turn" else s"-$turn"
            implicit val context = contextBuilder(
              transmittable.transmittables,
              messagingAbstraction derive directedTurn,
              direction)
            serializer serialize (transmittable send value)
          }

          def deserialize(value: MessageBuffer) = {
            val turn = receivingTurn.getAndIncrement
            val directedTurn = if (direction == sending) s"-$turn" else s"+$turn"
            implicit val context = contextBuilder(
              transmittable.transmittables,
              messagingAbstraction derive directedTurn,
              direction)
            (serializer deserialize value) map transmittable.receive
          }

          val endpoint = new Endpoint[B, R] {
            val receive = messagingAbstraction.channel.receive collect
              (Function unlift { deserialize(_).toOption })

            def send(value: B) =
              messagingAbstraction.channel send serialize(value)
          }
        }
      }
    }
  }

  implicit def delegating[D <: Delegating](implicit
      contextBuilders: ContextBuilders[Delegates[D]])
  : ContextBuilder[Delegates[D]] =
    new ContextBuilder[Delegates[D]] {
      def apply(
          transmittables: Delegates[D], abstraction: AbstractionRef,
          direction: Direction, index: Long) = {
        val context = contextBuilders(transmittables, abstraction, direction, index)
        new Context[Delegates[D]](transmittables, context.index, context) with
          Context.DelegatesNoEndpoint[D]
      }
    }

  implicit def none: ContextBuilder[None] =
    new ContextBuilder[None] {
      def apply(
          transmittables: None, abstraction: AbstractionRef,
          direction: Direction, index: Long) =
        new Context[None](transmittables, index, Contexts.None) with
          Context.NoneNoEndpoint
    }
}
