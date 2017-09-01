package loci
package transmitter
package dev

import scala.annotation.implicitNotFound

@implicitNotFound("Transmittable[${B}] not specified in: ${S}")
sealed trait Selector[
    B, I, R, T <: Transmittables, M <: Message.Transmittable,
    S <: Transmittables] {
  def transmittable(transmittables: S): Transmittable.Aux[B, I, R, T, M]
  def context(contexts: Contexts[S]): ContextBuilder.Context[T, M]
  def contextBuilder(contextBuilders: ContextBuilders[S]): ContextBuilder[T, M]
}

object Selector {
  implicit def single[
      B, I, R, T <: Transmittables, M <: Message.Transmittable] =
    new Selector[
        B, I, R, T, M,
        Transmittable.Aux[B, I, R, T, M]] {
      def transmittable(transmittables: Transmittable.Aux[B, I, R, T, M]) =
        transmittables
      def context(
          contexts: Contexts[Transmittable.Aux[B, I, R, T, M]]) =
        contexts.head
      def contextBuilder(
          contextBuilders: ContextBuilders[Transmittable.Aux[B, I, R, T, M]]) =
        contextBuilders.head
    }

  implicit def head[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      CR <: Transmittables] =
    new Selector[
        B, I, R, T, M,
        CR / Transmittable.Aux[B, I, R, T, M]] {
      def transmittable(transmittables: CR / Transmittable.Aux[B, I, R, T, M]) =
        transmittables.transmittable
      def context(
          contexts: Contexts[CR / Transmittable.Aux[B, I, R, T, M]]) =
        contexts.head
      def contextBuilder(
          contextBuilders: ContextBuilders[CR / Transmittable.Aux[B, I, R, T, M]]) =
        contextBuilders.head
    }

  implicit def tail[
      B, I, R, T <: Transmittables, M <: Message.Transmittable,
      B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable,
      CR <: Transmittables](
    implicit
      selector: Selector[B, I, R, T, M, CR]) =
    new Selector[
        B, I, R, T, M,
        CR / Transmittable.Aux[B0, I0, R0, T0, M0]] {
      def transmittable(transmittables: CR / Transmittable.Aux[B0, I0, R0, T0, M0]) =
        selector transmittable transmittables.rest
      def context(
          contexts: Contexts[CR / Transmittable.Aux[B0, I0, R0, T0, M0]]) =
        selector context contexts.tail
      def contextBuilder(
          contextBuilders: ContextBuilders[CR / Transmittable.Aux[B0, I0, R0, T0, M0]]) =
        selector contextBuilder contextBuilders.tail
    }
}
