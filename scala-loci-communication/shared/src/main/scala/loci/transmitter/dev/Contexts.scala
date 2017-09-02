package loci
package transmitter
package dev

import Transmittable.{ Delegating, Messaging }
import Transmittables.{ Delegates, Message }

sealed trait Contexts[T <: Transmittables] {
  def head[TH <: Transmittables]
    (implicit head: ContextsHead[T, TH]) = head(this)

  def tail[TT <: Transmittables]
    (implicit tail: ContextsTail[T, TT]) = tail(this)
}

object Contexts {
  object Empty extends Contexts[Delegates[NoDelegates]]

  final class SingleDelegating[
      B, I, R, T <: Transmittables] private[dev] (
      val context: ContextBuilder.Context[T])
    extends Contexts[Delegates[Transmittable.Aux[B, I, R, T]]]

  final class SingleMessage[
      B, I, R, T <: Transmittables] private[dev] (
      val context: ContextBuilder.Context[T])
    extends Contexts[Message[Transmittable.Aux[B, I, R, T]]]

  final class List[
      B, I, R, T <: Transmittables, TT <: Delegating] private[dev] (
      val context: ContextBuilder.Context[T],
      val contexts: Contexts[Delegates[TT]])
    extends Contexts[Delegates[TT / Transmittable.Aux[B, I, R, T]]]
}


sealed trait ContextsHead[T <: Transmittables, TH <: Transmittables] {
  def apply(contexts: Contexts[T]): ContextBuilder.Context[TH]
}

object ContextsHead {
  implicit def singleDelegating[
    B, I, R, T <: Transmittables, T0 <: Delegating](implicit
    ev: Contexts[Delegates[T0]] <:<
        Contexts[Delegates[Transmittable.Aux[B, I, R, T]]])
  : ContextsHead[Delegates[T0], T] =
    new ContextsHead[Delegates[T0], T] {
      def apply(contexts: Contexts[Delegates[T0]]) = ev(contexts) match {
        case contexts: Contexts.SingleDelegating[B, I, R, T] => contexts.context
        case _ => throw new TransmitterResolutionException(
          "Contexts[Delegates[T0]] <:< Contexts[Delegates[Transmittable[T]]]",
          "Delegates[Transmittable[T]]")
      }
    }

  implicit def singleMessage[
    B, I, R, T <: Transmittables, T0 <: Messaging](implicit
    ev: Contexts[Message[T0]] <:<
        Contexts[Message[Transmittable.Aux[B, I, R, T]]])
  : ContextsHead[Message[T0], T] =
    new ContextsHead[Message[T0], T] {
      def apply(contexts: Contexts[Message[T0]]) = ev(contexts) match {
        case contexts: Contexts.SingleMessage[B, I, R, T] => contexts.context
        case _ => throw new TransmitterResolutionException(
          "Contexts[Message[T0]] <:< Contexts[Message[Transmittable[T]]]",
          "Message[Transmittable[T]]")
      }
    }

  implicit def list[
    B, I, R, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: Contexts[Delegates[T0]] <:<
        Contexts[Delegates[TT / Transmittable.Aux[B, I, R, T]]])
  : ContextsHead[Delegates[T0], T] =
    new ContextsHead[Delegates[T0], T] {
      def apply(contexts: Contexts[Delegates[T0]]) = ev(contexts) match {
        case contexts: Contexts.List[B, I, R, T, TT] => contexts.context
        case _ => throw new TransmitterResolutionException(
          "Contexts[Delegates[T0]] <:< Contexts[Delegates[TT / Transmittable[T]]]",
          "Delegates[TT / Transmittable[T]]")
      }
    }
}


sealed trait ContextsTail[T <: Transmittables, TT <: Transmittables] {
  def apply(contexts: Contexts[T]): Contexts[TT]
}

object ContextsTail {
  implicit def list[
    B, I, R, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: Contexts[Delegates[T0]] <:<
        Contexts[Delegates[TT / Transmittable.Aux[B, I, R, T]]])
  : ContextsTail[Delegates[T0], Delegates[TT]] =
    new ContextsTail[Delegates[T0], Delegates[TT]] {
      def apply(contexts: Contexts[Delegates[T0]]) = ev(contexts) match {
        case contexts: Contexts.List[B, I, R, T, TT] => contexts.contexts
        case _ => throw new TransmitterResolutionException(
          "Contexts[Delegates[T0]] <:< Contexts[Delegates[TT / Transmittable[T]]]",
          "Delegates[TT / Transmittable[T]]")
      }
    }
}
