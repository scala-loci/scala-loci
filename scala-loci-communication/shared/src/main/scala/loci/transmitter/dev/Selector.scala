package loci
package transmitter
package dev

import scala.annotation.implicitNotFound

@implicitNotFound("${U} not specified in: ${T}")
sealed trait Selector[U <: Transmittable[_], T <: Transmittables] {
  def context(transmittable: U, contexts: Contexts[T])
    : ContextBuilder.Context[transmittable.Transmittables, transmittable.Message]
  def contextBuilder(transmittable: U, contextBuilders: ContextBuilders[T])
    : ContextBuilder[transmittable.Transmittables, transmittable.Message]
}

object Selector {
  implicit def single[
      B, I, R, T <: Transmittables, M <: Message.Transmittable] =
    new Selector[
        Transmittable.Aux[B, I, R, T, M],
        Transmittable.Aux[B, I, R, T, M]] {
      def context(
          transmittable: Transmittable.Aux[B, I, R, T, M],
          contexts: Contexts[Transmittable.Aux[B, I, R, T, M]]) =
        contexts.head
      def contextBuilder(
          transmittable: Transmittable.Aux[B, I, R, T, M],
          contextBuilders: ContextBuilders[Transmittable.Aux[B, I, R, T, M]]) =
        contextBuilders.head
    }

  implicit def head[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      CR <: Transmittables] =
    new Selector[
        Transmittable.Aux[B, I, R, T, M],
        CR / Transmittable.Aux[B, I, R, T, M]] {
      def context(
          transmittable: Transmittable.Aux[B, I, R, T, M],
          contexts: Contexts[CR / Transmittable.Aux[B, I, R, T, M]]) =
        contexts.head
      def contextBuilder(
          transmittable: Transmittable.Aux[B, I, R, T, M],
          contextBuilders: ContextBuilders[CR / Transmittable.Aux[B, I, R, T, M]]) =
        contextBuilders.head
    }

  implicit def tail[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable,
      CR <: Transmittables](
    implicit
      selector: Selector[Transmittable.Aux[B, I, R, T, M], CR]) =
    new Selector[
        Transmittable.Aux[B, I, R, T, M],
        CR / Transmittable.Aux[B0, I0, R0, T0, M0]] {
      def context(
          transmittable: Transmittable.Aux[B, I, R, T, M],
          contexts: Contexts[CR / Transmittable.Aux[B0, I0, R0, T0, M0]]) =
        selector context (transmittable, contexts.tail)
      def contextBuilder(
          transmittable: Transmittable.Aux[B, I, R, T, M],
          contextBuilders: ContextBuilders[CR / Transmittable.Aux[B0, I0, R0, T0, M0]]) =
        selector contextBuilder (transmittable, contextBuilders.tail)
    }
}
