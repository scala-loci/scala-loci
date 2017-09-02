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

  final case class SingleDelegating[
      B, I, R, T <: Transmittables] private[dev] (
      context: ContextBuilder.Context[T])
    extends Contexts[Delegates[Transmittable.Aux[B, I, R, T]]]

  final case class SingleMessage[
      B, I, R, T <: Transmittables] private[dev] (
      context: ContextBuilder.Context[T])
    extends Contexts[Message[Transmittable.Aux[B, I, R, T]]]

  final case class List[
      B, I, R, T <: Transmittables, TT <: Delegating] private[dev] (
      contextHead: ContextBuilder.Context[T],
      contextTail: Contexts[Delegates[TT]])
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
      def apply(contexts: Contexts[Delegates[T0]]) = {
        val Contexts.SingleDelegating(context) = ev(contexts)
        context
      }
    }

  implicit def singleMessage[
    B, I, R, T <: Transmittables, T0 <: Messaging](implicit
    ev: Contexts[Message[T0]] <:<
        Contexts[Message[Transmittable.Aux[B, I, R, T]]])
  : ContextsHead[Message[T0], T] =
    new ContextsHead[Message[T0], T] {
      def apply(contexts: Contexts[Message[T0]]) = {
        val Contexts.SingleMessage(context) = ev(contexts)
        context
      }
    }

  implicit def list[
    B, I, R, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: Contexts[Delegates[T0]] <:<
        Contexts[Delegates[TT / Transmittable.Aux[B, I, R, T]]])
  : ContextsHead[Delegates[T0], T] =
    new ContextsHead[Delegates[T0], T] {
      def apply(contexts: Contexts[Delegates[T0]]) = {
        val Contexts.List(contextHead, _) = ev(contexts)
        contextHead
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
      def apply(contexts: Contexts[Delegates[T0]]) = {
        val Contexts.List(_, contextTail) = ev(contexts)
        contextTail
      }
    }
}
