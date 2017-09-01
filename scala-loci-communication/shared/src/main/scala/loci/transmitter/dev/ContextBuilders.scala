package loci
package transmitter
package dev

sealed trait ContextBuilders[CT <: Transmittables] {
  def head[T <: Transmittables, M <: Message.Transmittable]
    (implicit head: ContextBuildersHead[CT, T, M]) = head(this)

  def tail[CR <: Transmittables]
    (implicit tail: ContextBuildersTail[CT, CR]) = tail(this)

  def apply(transmittables: CT, abstraction: AbstractionRef): Contexts[CT]
}

object ContextBuilders {
  implicit object empty extends ContextBuilders[NoTransmittables] {
    def apply(transmittables: NoTransmittables, abstraction: AbstractionRef) =
      Contexts.Empty
  }

  implicit def single[
      B, I, R, T <: Transmittables, M <: Message.Transmittable](
      implicit
        contextBuilder: ContextBuilder[T, M]) =
    new Single[B, I, R, T, M](contextBuilder)

  implicit def list[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      CR <: Transmittables](
      implicit
        contextBuilder: ContextBuilder[T, M],
        contextBuilders: ContextBuilders[CR]) =
    new List[B, I, R, T, M, CR](contextBuilder, contextBuilders)

  class Single[
      B, I, R, T <: Transmittables, M <: Message.Transmittable
    ] private[dev] (
      val contextBuilder: ContextBuilder[T, M])
    extends ContextBuilders[Transmittable.Aux[B, I, R, T, M]] {
      def apply(
          transmittables: Transmittable.Aux[B, I, R, T, M],
          abstraction: AbstractionRef) =
        new Contexts.Single(
          contextBuilder(transmittables.transmittables, abstraction))
  }

  class List[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      CR <: Transmittables
    ] private[dev] (
      val contextBuilder: ContextBuilder[T, M],
      val contextBuilders: ContextBuilders[CR])
    extends ContextBuilders[CR / Transmittable.Aux[B, I, R, T, M]] {
      def apply(
          transmittables: CR / Transmittable.Aux[B, I, R, T, M],
          abstraction: AbstractionRef) =
        new Contexts.List(
          contextBuilder(transmittables.transmittable.transmittables, abstraction),
          contextBuilders(transmittables.rest, abstraction))
  }
}


sealed trait ContextBuildersHead[
    CT <: Transmittables, T <: Transmittables, M <: Message.Transmittable] {
  def apply(contextBuilders: ContextBuilders[CT]): ContextBuilder[T, M]
}

object ContextBuildersHead {
  implicit def single[
      CT <: Transmittables, B, I, R,
      T <: Transmittables, M <: Message.Transmittable](
    implicit
      ev: ContextBuilders[CT]
          <:< ContextBuilders[Transmittable.Aux[B, I, R, T, M]]) =
    new ContextBuildersHead[CT, T, M] {
      def apply(contextBuilders: ContextBuilders[CT]) = ev(contextBuilders) match {
        case contextBuilders: ContextBuilders.Single[B, I, R, T, M] =>
          contextBuilders.contextBuilder
        case _ => throw new TransmitterResolutionException(
          "ContextBuilders[CT] <:< ContextBuilders[Transmittable[T]]",
          "Transmittable.Aux")
      }
    }

  implicit def list[
      CT <: Transmittables, B, I, R,
      T <: Transmittables, M <: Message.Transmittable, CR <: Transmittables](
    implicit
      ev: ContextBuilders[CT]
          <:< ContextBuilders[CR / Transmittable.Aux[B, I, R, T, M]]) =
    new ContextBuildersHead[CT, T, M] {
      def apply(contextBuilders: ContextBuilders[CT]) =
        ev(contextBuilders) match {
          case contextBuilders: ContextBuilders.List[B, I, R, T, M, CR] =>
            contextBuilders.contextBuilder
          case _ => throw new TransmitterResolutionException(
            "ContextBuilders[CT] <:< ContextBuilders[CR / Transmittable[T]]",
            "CR / Transmittable")
        }
    }
}


sealed trait ContextBuildersTail[CT <: Transmittables, CR <: Transmittables] {
  def apply(contextBuilders: ContextBuilders[CT]): ContextBuilders[CR]
}

object ContextBuildersTail {
  implicit def list[
      CT <: Transmittables, B, I, R,
      T <: Transmittables, M <: Message.Transmittable, CR <: Transmittables](
    implicit
      ev: ContextBuilders[CT]
          <:< ContextBuilders[CR / Transmittable.Aux[B, I, R, T, M]]) =
    new ContextBuildersTail[CT, CR] {
      def apply(contextBuilders: ContextBuilders[CT]) =
        ev(contextBuilders) match {
          case contextBuilders: ContextBuilders.List[B, I, R, T, M, CR] =>
            contextBuilders.contextBuilders
          case _ => throw new TransmitterResolutionException(
            "ContextBuilders[CT] <:< ContextBuilders[CR / Transmittable[T]]",
            "CR / Transmittable")
        }
    }
}
