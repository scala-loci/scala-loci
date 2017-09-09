package loci
package transmitter
package dev

import Transmittable.{ Delegating, Messaging }
import Transmittables.{ Delegates, Message }

sealed trait ContextBuilders[T <: Transmittables] {
  def head[TH <: Transmittables]
    (implicit head: ContextBuildersHead[T, TH]) = head(this)

  def tail[TT <: Transmittables]
    (implicit tail: ContextBuildersTail[T, TT]) = tail(this)

  def apply(transmittables: T, abstraction: AbstractionRef): Contexts[T]
}

object ContextBuilders {
  implicit object empty extends ContextBuilders[Delegates[NoDelegates]] {
    def apply(transmittables: Delegates[NoDelegates], abstraction: AbstractionRef) =
      Contexts.Empty
  }

  implicit def singleDelegating[B, I, R, P, T <: Transmittables](implicit
      contextBuilder: ContextBuilder[T]): SingleDelegating[B, I, R, P, T] =
    SingleDelegating[B, I, R, P, T](contextBuilder)

  implicit def singleMessage[B, I, R, P, T <: Transmittables](implicit
      contextBuilder: ContextBuilder[T]): SingleMessage[B, I, R, P, T] =
    SingleMessage[B, I, R, P, T](contextBuilder)

  implicit def list[B, I, R, P, T <: Transmittables, TT <: Delegating](implicit
      contextBuilderHead: ContextBuilder[T],
      contextBuilderTail: ContextBuilders[Delegates[TT]]): List[B, I, R, P, T, TT] =
    List[B, I, R, P, T, TT](contextBuilderHead, contextBuilderTail)


  final case class SingleDelegating[
      B, I, R, P, T <: Transmittables] private[dev] (
      contextBuilder: ContextBuilder[T])
    extends ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]] {
      def apply(
          transmittables: Delegates[Transmittable.Aux[B, I, R, P, T]],
          abstraction: AbstractionRef) =
        Contexts.SingleDelegating(
          contextBuilder(transmittables.delegates.transmittables, abstraction))
  }

  final case class SingleMessage[
      B, I, R, P, T <: Transmittables] private[dev] (
      contextBuilder: ContextBuilder[T])
    extends ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]] {
      def apply(
          transmittables: Message[Transmittable.Aux[B, I, R, P, T]],
          abstraction: AbstractionRef) =
        Contexts.SingleMessage(
          contextBuilder(transmittables.message.transmittables, abstraction))
  }

  final case class List[
      B, I, R, P, T <: Transmittables, TT <: Delegating] private[dev] (
      contextBuilder: ContextBuilder[T],
      contextBuilders: ContextBuilders[Delegates[TT]])
    extends ContextBuilders[Delegates[TT / Transmittable.Aux[B, I, R, P, T]]] {
      def apply(
          transmittables: Delegates[TT / Transmittable.Aux[B, I, R, P, T]],
          abstraction: AbstractionRef) =
        Contexts.List(
          contextBuilder(transmittables.delegates.head.transmittables, abstraction),
          contextBuilders(transmittables.delegates.tailDelegates, abstraction))
  }
}


sealed trait ContextBuildersHead[T <: Transmittables, TH <: Transmittables] {
  def apply(contextBuilders: ContextBuilders[T]): ContextBuilder[TH]
}

object ContextBuildersHead {
  implicit def singleDelegating[
    B, I, R, P, T <: Transmittables, T0 <: Delegating](implicit
    ev: ContextBuilders[Delegates[T0]] <:<
        ContextBuilders[Delegates[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuildersHead[Delegates[T0], T] =
    new ContextBuildersHead[Delegates[T0], T] {
      def apply(contextBuilders: ContextBuilders[Delegates[T0]]) = {
        val ContextBuilders.SingleDelegating(contextBuilder) = ev(contextBuilders)
        contextBuilder
      }
    }

  implicit def singleMessage[
    B, I, R, P, T <: Transmittables, T0 <: Messaging](implicit
    ev: ContextBuilders[Message[T0]] <:<
        ContextBuilders[Message[Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuildersHead[Message[T0], T] =
    new ContextBuildersHead[Message[T0], T] {
      def apply(contextBuilders: ContextBuilders[Message[T0]]) = {
        val ContextBuilders.SingleMessage(contextBuilder) = ev(contextBuilders)
        contextBuilder
      }
    }

  implicit def list[
    B, I, R, P, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: ContextBuilders[Delegates[T0]] <:<
        ContextBuilders[Delegates[TT / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuildersHead[Delegates[T0], T] =
    new ContextBuildersHead[Delegates[T0], T] {
      def apply(contextBuilders: ContextBuilders[Delegates[T0]]) = {
        val ContextBuilders.List(contextBuilderHead, _) = ev(contextBuilders)
        contextBuilderHead
      }
    }
}


sealed trait ContextBuildersTail[T <: Transmittables, TT <: Transmittables] {
  def apply(contextBuilders: ContextBuilders[T]): ContextBuilders[TT]
}

object ContextBuildersTail {
  implicit def list[
    B, I, R, P, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: ContextBuilders[Delegates[T0]] <:<
        ContextBuilders[Delegates[TT / Transmittable.Aux[B, I, R, P, T]]])
  : ContextBuildersTail[Delegates[T0], Delegates[TT]] =
    new ContextBuildersTail[Delegates[T0], Delegates[TT]] {
      def apply(contextBuilders: ContextBuilders[Delegates[T0]]) = {
        val ContextBuilders.List(_, contextBuilderTail) = ev(contextBuilders)
        contextBuilderTail
      }
    }
}
