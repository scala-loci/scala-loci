package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message }
import scala.annotation.implicitNotFound

@implicitNotFound("Transmittable[${B}, ${I}, ${R}] not specified in: ${S}")
sealed trait Selector[B, I, R, P, T <: Transmittables, S <: Transmittables] {
  def transmittable(transmittables: S): Transmittable.Aux[B, I, R, P, T]
  def context(contexts: Contexts[S]): ContextBuilder.Context[T]
  def contextBuilder(contextBuilders: ContextBuilders[S]): ContextBuilder[T]
}

object Selector {
  implicit def message[B, I, R, P, T <: Transmittables]
  : Selector[B, I, R, P, T, Message[Transmittable.Aux[B, I, R, P, T]]] = {
    type S = Message[Transmittable.Aux[B, I, R, P, T]]

    new Selector[B, I, R, P, T, S] {
      def transmittable(transmittables: S) =
        transmittables.message
      def context(contexts: Contexts[S]) =
        contexts.message
      def contextBuilder(contextBuilders: ContextBuilders[S]) =
        contextBuilders.message
    }
  }

  implicit def delegate[B, I, R, P, T <: Transmittables]
  : Selector[B, I, R, P, T, Delegates[Transmittable.Aux[B, I, R, P, T]]] = {
    type S = Delegates[Transmittable.Aux[B, I, R, P, T]]

    new Selector[B, I, R, P, T, S] {
      def transmittable(transmittables: S) =
        transmittables.delegates
      def context(contexts: Contexts[S]) =
        contexts.delegate
      def contextBuilder(contextBuilders: ContextBuilders[S]) =
        contextBuilders.delegate
    }
  }

  implicit def head[B, I, R, P, T <: Transmittables, D <: Delegating]
  : Selector[B, I, R, P, T, Delegates[D / Transmittable.Aux[B, I, R, P, T]]] = {
    type S = Delegates[D / Transmittable.Aux[B, I, R, P, T]]

    new Selector[B, I, R, P, T, S] {
      def transmittable(transmittables: S) =
        transmittables.delegates.head
      def context(contexts: Contexts[S]) =
        contexts.delegatesHead
      def contextBuilder(contextBuilders: ContextBuilders[S]) =
        contextBuilders.delegatesHead
    }
  }

  implicit def tail[
      B, I, R, P, T <: Transmittables,
      B0, I0, R0, P0, T0 <: Transmittables, D <: Delegating](implicit
      selector: Selector[B, I, R, P, T, Delegates[D]])
  : Selector[B, I, R, P, T, Delegates[D / Transmittable.Aux[B0, I0, R0, P0, T0]]] = {
    type S = Delegates[D / Transmittable.Aux[B0, I0, R0, P0, T0]]

    new Selector[B, I, R, P, T, S] {
      def transmittable(transmittables: S) =
        selector transmittable transmittables.delegates.tailDelegates
      def context(contexts: Contexts[S]) =
        selector context contexts.delegatesTail
      def contextBuilder(contextBuilders: ContextBuilders[S]) =
        selector contextBuilder contextBuilders.delegatesTail
    }
  }
}
