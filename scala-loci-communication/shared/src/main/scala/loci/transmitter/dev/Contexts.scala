package loci
package transmitter
package dev

sealed trait Contexts[CT <: Transmittables] {
  def head[T <: Transmittables, M <: Message.Transmittable]
    (implicit head: ContextsHead[CT, T, M]) = head(this)

  def tail[CR <: Transmittables]
    (implicit tail: ContextsTail[CT, CR]) = tail(this)
}

object Contexts {
  object Empty extends Contexts[NoTransmittables]

  final class Single[
      B, I, R, T <: Transmittables, M <: Message.Transmittable
    ] private[dev] (
      val context: ContextBuilder.Context[T, M])
    extends Contexts[Transmittable.Aux[B, I, R, T, M]]

  final class List[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      CR <: Transmittables
    ] private[dev] (
      val context: ContextBuilder.Context[T, M],
      val contexts: Contexts[CR])
    extends Contexts[CR / Transmittable.Aux[B, I, R, T, M]]
}


sealed trait ContextsHead[
    CT <: Transmittables, T <: Transmittables, M <: Message.Transmittable] {
  def apply(contexts: Contexts[CT]): ContextBuilder.Context[T, M]
}

object ContextsHead {
  implicit def single[
      CT <: Transmittables, B, I, R,
      T <: Transmittables, M <: Message.Transmittable](
    implicit
      ev: Contexts[CT] <:< Contexts[Transmittable.Aux[B, I, R, T, M]]) =
    new ContextsHead[CT, T, M] {
      def apply(contexts: Contexts[CT]) = ev(contexts) match {
        case contexts: Contexts.Single[B, I, R, T, M] => contexts.context
        case _ => throw new TransmitterResolutionException(
          "Contexts[CT] <:< Contexts[Transmittable[T]]",
          "Transmittable.Aux")
      }
    }

  implicit def list[
      CT <: Transmittables, B, I, R,
      T <: Transmittables, M <: Message.Transmittable, CR <: Transmittables](
    implicit
      ev: Contexts[CT] <:< Contexts[CR / Transmittable.Aux[B, I, R, T, M]]) =
    new ContextsHead[CT, T, M] {
      def apply(contexts: Contexts[CT]) = ev(contexts) match {
        case contexts: Contexts.List[B, I, R, T, M, CR] => contexts.context
        case _ => throw new TransmitterResolutionException(
          "Contexts[CT] <:< Contexts[CR / Transmittable[T]]",
          "CR / Transmittable")
      }
    }
}


sealed trait ContextsTail[CT <: Transmittables, CR <: Transmittables] {
  def apply(contexts: Contexts[CT]): Contexts[CR]
}

object ContextsTail {
  implicit def list[
      CT <: Transmittables, B, I, R,
      T <: Transmittables, M <: Message.Transmittable, CR <: Transmittables](
    implicit
      ev: Contexts[CT] <:< Contexts[CR / Transmittable.Aux[B, I, R, T, M]]) =
    new ContextsTail[CT, CR] {
      def apply(contexts: Contexts[CT]) = ev(contexts) match {
        case contexts: Contexts.List[B, I, R, T, M, CR] => contexts.contexts
        case _ => throw new TransmitterResolutionException(
          "Contexts[CT] <:< Contexts[CR / Transmittable[T]]",
          "CR / Transmittable")
      }
    }
}
