package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message }

sealed trait ContextBuilders[S <: Transmittables] {
  import ContextBuilders._

  def message[B, I, R, P, T <: Transmittables](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder[T] =
    ev(this) match { case message: SingleMessage[B, I, R, P, T] => message.builder }

  def delegate[B, I, R, P, T <: Transmittables](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder[T] =
    ev(this) match { case delegating: SingleDelegate[B, I, R, P, T] => delegating.builder }

  def delegatesHead[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder[T] =
    ev(this) match { case list: List[B, I, R, P, T, D] => list.builderHead }

  def delegatesTail[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    ev: ContextBuilders[S] <:< ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilders[Delegates[D]] =
    ev(this) match { case list: List[B, I, R, P, T, D] => list.builderTail }

  def apply(transmittables: S, abstraction: AbstractionRef): Contexts[S]
}

object ContextBuilders {
  final class SingleMessage[B, I, R, P, T <: Transmittables] private[dev] (
      val builder: ContextBuilder[T])
    extends ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]] {

    def apply(
        transmittables: Message[Transmittable.Aux[B, I, R, P, T]],
        abstraction: AbstractionRef) =
      new Contexts.SingleMessage(
        builder(transmittables.message.transmittables, abstraction))
  }

  final class SingleDelegate[B, I, R, P, T <: Transmittables] private[dev] (
      val builder: ContextBuilder[T])
    extends ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]] {

    def apply(
        transmittables: Delegates[Transmittable.Aux[B, I, R, P, T]],
        abstraction: AbstractionRef) =
      new Contexts.SingleDelegate(
        builder(transmittables.delegates.transmittables, abstraction))
  }

  final class List[B, I, R, P, T <: Transmittables, D <: Delegating] private[dev] (
      val builderHead: ContextBuilder[T],
      val builderTail: ContextBuilders[Delegates[D]])
    extends ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]] {

    def apply(
        transmittables: Delegates[D / Transmittable.Aux[B, I, R, P, T]],
        abstraction: AbstractionRef) =
      new Contexts.List(
        builderHead(transmittables.delegates.head.transmittables, abstraction),
        builderTail(transmittables.delegates.tailDelegates, abstraction))
  }

  implicit def message[B, I, R, P, T <: Transmittables](implicit
    contextBuilder: ContextBuilder[T])
  : ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]] =
    new SingleMessage(contextBuilder)

  implicit def delegate[B, I, R, P, T <: Transmittables](implicit
    contextBuilder: ContextBuilder[T])
  : ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]] =
    new SingleDelegate(contextBuilder)

  implicit def list[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    contextBuilder: ContextBuilder[T],
    contextBuilders: ContextBuilders[Delegates[D]])
  : ContextBuilders[Delegates[D / Transmittable.Aux[B, I, R, P, T]]] =
    new List(contextBuilder, contextBuilders)
}
