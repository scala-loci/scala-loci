package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message, None }

sealed trait Contexts[S <: Transmittables] {
  import Contexts._

  val index: Long

  def message[B, I, R, P, T <: Transmittables](implicit
    ev: Contexts[S] <:< Contexts[Message[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder.Context[T] =
    ev(this) match { case message: SingleMessage[B, I, R, P, T] => message.context }

  def delegate[B, I, R, P, T <: Transmittables](implicit
    ev: Contexts[S] <:< Contexts[Delegates[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder.Context[T] =
    ev(this) match { case delegating: SingleDelegate[B, I, R, P, T] => delegating.context }

  def delegatesHead[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    ev: Contexts[S] <:< Contexts[Delegates[D / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuilder.Context[T] =
    ev(this) match { case list: List[B, I, R, P, T, D] => list.contextHead }

  def delegatesTail[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
    ev: Contexts[S] <:< Contexts[Delegates[D / Transmittable.Aux[B, I, R, P, T]]])
  : Contexts[Delegates[D]] =
    ev(this) match { case list: List[B, I, R, P, T, D] => list.contextTail }
}

object Contexts {
  object None extends Contexts[None] { val index = 0l }

  final class SingleMessage[B, I, R, P, T <: Transmittables] private[dev] (
      val context: ContextBuilder.Context[T],
      val index: Long)
    extends Contexts[Message[Transmittable.Aux[B, I, R, P, T]]]

  final class SingleDelegate[B, I, R, P, T <: Transmittables] private[dev] (
      val context: ContextBuilder.Context[T],
      val index: Long)
    extends Contexts[Delegates[Transmittable.Aux[B, I, R, P, T]]]

  final class List[B, I, R, P, T <: Transmittables, D <: Delegating] private[dev] (
      val contextHead: ContextBuilder.Context[T],
      val contextTail: Contexts[Delegates[D]],
      val index: Long)
    extends Contexts[Delegates[D / Transmittable.Aux[B, I, R, P, T]]]
}
