package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message }
import scala.annotation.implicitNotFound

@implicitNotFound("Transmittable[${B}] not specified in: ${S}")
sealed trait Selector[
    B, I, R, P, T <: Transmittables, S <: Transmittables] {
  def transmittable(transmittables: S): Transmittable.Aux[B, I, R, P, T]
  def context(contexts: Contexts[S]): ContextBuilder.Context[T]
  def contextBuilder(contextBuilders: ContextBuilders[S]): ContextBuilder[T]
}

object Selector {
  implicit def singleDelegating[B, I, R, P, T <: Transmittables]
  : Selector[B, I, R, P, T, Delegates[Transmittable.Aux[B, I, R, P, T]]] = {
    type D = Delegates[Transmittable.Aux[B, I, R, P, T]]
    
    new Selector[B, I, R, P, T, D] {
      def transmittable(transmittables: D) =
        transmittables.delegates
      def context(contexts: Contexts[D]) =
        contexts.head
      def contextBuilder(contextBuilders: ContextBuilders[D]) =
        contextBuilders.head
    }
  }

  implicit def singleMessage[B, I, R, P, T <: Transmittables]
  : Selector[B, I, R, P, T, Message[Transmittable.Aux[B, I, R, P, T]]] = {
    type M = Message[Transmittable.Aux[B, I, R, P, T]]
    
    new Selector[B, I, R, P, T, M] {
      def transmittable(transmittables: M) =
        transmittables.message
      def context(contexts: Contexts[M]) =
        contexts.head
      def contextBuilder(contextBuilders: ContextBuilders[M]) =
        contextBuilders.head
    }
  }

  implicit def head[B, I, R, P, T <: Transmittables, TT <: Delegating]
  : Selector[B, I, R, P, T, Delegates[TT / Transmittable.Aux[B, I, R, P, T]]] = {
    type D = Delegates[TT / Transmittable.Aux[B, I, R, P, T]]
    
    new Selector[B, I, R, P, T, D] {
      def transmittable(transmittables: D) =
        transmittables.delegates.head
      def context(contexts: Contexts[D]) =
        contexts.head
      def contextBuilder(contextBuilders: ContextBuilders[D]) =
        contextBuilders.head
    }
  }

  implicit def tail[
      B, I, R, P, T <: Transmittables,
      B0, I0, R0, P0, T0 <: Transmittables, TT <: Delegating](implicit
      selector: Selector[B, I, R, P, T, Delegates[TT]])
  : Selector[B, I, R, P, T, Delegates[TT / Transmittable.Aux[B0, I0, R0, P0, T0]]] = {
    type D = Delegates[TT / Transmittable.Aux[B0, I0, R0, P0, T0]]
    
    new Selector[B, I, R, P, T, D] {
      def transmittable(transmittables: D) =
        selector transmittable transmittables.delegates.tailDelegates
      def context(contexts: Contexts[D]) =
        selector context contexts.tail
      def contextBuilder(contextBuilders: ContextBuilders[D]) =
        selector contextBuilder contextBuilders.tail
    }
  }
}
