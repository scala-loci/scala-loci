package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message }
import scala.annotation.implicitNotFound

@implicitNotFound("Transmittable[${B}] not specified in: ${S}")
sealed trait Selector[
    B, I, R, T <: Transmittables, S <: Transmittables] {
  def transmittable(transmittables: S): Transmittable.Aux[B, I, R, T]
  def context(contexts: Contexts[S]): ContextBuilder.Context[T]
  def contextBuilder(contextBuilders: ContextBuilders[S]): ContextBuilder[T]
}

object Selector {
  implicit def singleDelegating[B, I, R, T <: Transmittables]
  : Selector[B, I, R, T, Delegates[Transmittable.Aux[B, I, R, T]]] = {
    type D = Delegates[Transmittable.Aux[B, I, R, T]]
    
    new Selector[B, I, R, T, D] {
      def transmittable(transmittables: D) =
        transmittables.delegates
      def context(contexts: Contexts[D]) =
        contexts.head
      def contextBuilder(contextBuilders: ContextBuilders[D]) =
        contextBuilders.head
    }
  }

  implicit def singleMessage[B, I, R, T <: Transmittables]
  : Selector[B, I, R, T, Message[Transmittable.Aux[B, I, R, T]]] = {
    type M = Message[Transmittable.Aux[B, I, R, T]]
    
    new Selector[B, I, R, T, M] {
      def transmittable(transmittables: M) =
        transmittables.message
      def context(contexts: Contexts[M]) =
        contexts.head
      def contextBuilder(contextBuilders: ContextBuilders[M]) =
        contextBuilders.head
    }
  }

  implicit def head[B, I, R, T <: Transmittables, TT <: Delegating]
  : Selector[B, I, R, T, Delegates[TT / Transmittable.Aux[B, I, R, T]]] = {
    type D = Delegates[TT / Transmittable.Aux[B, I, R, T]]
    
    new Selector[B, I, R, T, D] {
      def transmittable(transmittables: D) =
        transmittables.delegates.head
      def context(contexts: Contexts[D]) =
        contexts.head
      def contextBuilder(contextBuilders: ContextBuilders[D]) =
        contextBuilders.head
    }
  }

  implicit def tail[
      B, I, R, T <: Transmittables,
      B0, I0, R0, T0 <: Transmittables, TT <: Delegating](implicit
      selector: Selector[B, I, R, T, Delegates[TT]])
  : Selector[B, I, R, T, Delegates[TT / Transmittable.Aux[B0, I0, R0, T0]]] = {
    type D = Delegates[TT / Transmittable.Aux[B0, I0, R0, T0]]
    
    new Selector[B, I, R, T, D] {
      def transmittable(transmittables: D) =
        selector transmittable transmittables.delegates.tailDelegates
      def context(contexts: Contexts[D]) =
        selector context contexts.tail
      def contextBuilder(contextBuilders: ContextBuilders[D]) =
        selector contextBuilder contextBuilders.tail
    }
  }
}
