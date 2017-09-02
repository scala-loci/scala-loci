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

  implicit def singleDelegating[B, I, R, T <: Transmittables](implicit
      contextBuilder: ContextBuilder[T]): SingleDelegating[B, I, R, T] =
    new SingleDelegating[B, I, R, T](contextBuilder)

  implicit def singleMessage[B, I, R, T <: Transmittables](implicit
      contextBuilder: ContextBuilder[T]): SingleMessage[B, I, R, T] =
    new SingleMessage[B, I, R, T](contextBuilder)

  implicit def list[B, I, R, T <: Transmittables, TT <: Delegating](implicit
      contextBuilder: ContextBuilder[T],
      contextBuilders: ContextBuilders[Delegates[TT]]): List[B, I, R, T, TT] =
    new List[B, I, R, T, TT](contextBuilder, contextBuilders)


  final class SingleDelegating[
      B, I, R, T <: Transmittables] private[dev] (
      val contextBuilder: ContextBuilder[T])
    extends ContextBuilders[Delegates[Transmittable.Aux[B, I, R, T]]] {
      def apply(
          transmittables: Delegates[Transmittable.Aux[B, I, R, T]],
          abstraction: AbstractionRef) =
        new Contexts.SingleDelegating(
          contextBuilder(transmittables.delegates.transmittables, abstraction))
  }

  final class SingleMessage[
      B, I, R, T <: Transmittables] private[dev] (
      val contextBuilder: ContextBuilder[T])
    extends ContextBuilders[Message[Transmittable.Aux[B, I, R, T]]] {
      def apply(
          transmittables: Message[Transmittable.Aux[B, I, R, T]],
          abstraction: AbstractionRef) =
        new Contexts.SingleMessage(
          contextBuilder(transmittables.message.transmittables, abstraction))
  }

  final class List[
      B, I, R, T <: Transmittables, TT <: Delegating] private[dev] (
      val contextBuilder: ContextBuilder[T],
      val contextBuilders: ContextBuilders[Delegates[TT]])
    extends ContextBuilders[Delegates[TT / Transmittable.Aux[B, I, R, T]]] {
      def apply(
          transmittables: Delegates[TT / Transmittable.Aux[B, I, R, T]],
          abstraction: AbstractionRef) =
        new Contexts.List(
          contextBuilder(transmittables.delegates.head.transmittables, abstraction),
          contextBuilders(transmittables.delegates.tailDelegates, abstraction))
  }
}


sealed trait ContextBuildersHead[T <: Transmittables, TH <: Transmittables] {
  def apply(contextBuilders: ContextBuilders[T]): ContextBuilder[TH]
}

object ContextBuildersHead {
  implicit def singleDelegating[
    B, I, R, T <: Transmittables, T0 <: Delegating](implicit
    ev: ContextBuilders[Delegates[T0]] <:<
        ContextBuilders[Delegates[Transmittable.Aux[B, I, R, T]]])
  : ContextBuildersHead[Delegates[T0], T] =
    new ContextBuildersHead[Delegates[T0], T] {
      def apply(contextBuilders: ContextBuilders[Delegates[T0]]) =
        ev(contextBuilders) match {
          case contextBuilders: ContextBuilders.SingleDelegating[B, I, R, T] =>
            contextBuilders.contextBuilder
          case _ => throw new TransmitterResolutionException(
            "ContextBuilders[Delegates[T0]] <:< ContextBuilders[Delegates[Transmittable[T]]]",
            "Delegates[Transmittable[T]]")
        }
    }

  implicit def singleMessage[
    B, I, R, T <: Transmittables, T0 <: Messaging](implicit
    ev: ContextBuilders[Message[T0]] <:<
        ContextBuilders[Message[Transmittable.Aux[B, I, R, T]]])
  : ContextBuildersHead[Message[T0], T] =
    new ContextBuildersHead[Message[T0], T] {
      def apply(contextBuilders: ContextBuilders[Message[T0]]) =
        ev(contextBuilders) match {
          case contextBuilders: ContextBuilders.SingleMessage[B, I, R, T] =>
            contextBuilders.contextBuilder
          case _ => throw new TransmitterResolutionException(
            "ContextBuilders[Message[T0]] <:< ContextBuilders[Message[Transmittable[T]]]",
            "Message[Transmittable[T]]")
        }
    }

  implicit def list[
    B, I, R, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: ContextBuilders[Delegates[T0]] <:<
        ContextBuilders[Delegates[TT / Transmittable.Aux[B, I, R, T]]])
  : ContextBuildersHead[Delegates[T0], T] =
    new ContextBuildersHead[Delegates[T0], T] {
      def apply(contextBuilders: ContextBuilders[Delegates[T0]]) =
        ev(contextBuilders) match {
          case contextBuilders: ContextBuilders.List[B, I, R, T, TT] =>
            contextBuilders.contextBuilder
          case _ => throw new TransmitterResolutionException(
            "ContextBuilders[Delegates[T0]] <:< ContextBuilders[Delegates[TT / Transmittable[T]]]",
            "Delegates[TT / Transmittable[T]]")
        }
    }
}


sealed trait ContextBuildersTail[T <: Transmittables, TT <: Transmittables] {
  def apply(contextBuilders: ContextBuilders[T]): ContextBuilders[TT]
}

object ContextBuildersTail {
  implicit def list[
    B, I, R, T <: Transmittables, T0 <: Delegating, TT <: Delegating](implicit
    ev: ContextBuilders[Delegates[T0]] <:<
        ContextBuilders[Delegates[TT / Transmittable.Aux[B, I, R, T]]])
  : ContextBuildersTail[Delegates[T0], Delegates[TT]] =
    new ContextBuildersTail[Delegates[T0], Delegates[TT]] {
      def apply(contextBuilders: ContextBuilders[Delegates[T0]]) =
        ev(contextBuilders) match {
          case contextBuilders: ContextBuilders.List[B, I, R, T, TT] =>
            contextBuilders.contextBuilders
          case _ => throw new TransmitterResolutionException(
            "ContextBuilders[Delegates[T0]] <:< ContextBuilders[Delegates[TT / Transmittable[T]]]",
            "Delegates[TT / Transmittable[T]]")
        }
    }
}
