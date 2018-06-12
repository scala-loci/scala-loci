package loci
package transmitter
package dev

import Transmittable.Delegating
import Transmittables.{ Delegates, Message, None }


trait Context[S <: Transmittables] {
  this: ContextBuilder.Context[S] =>

  def endpoint[B, I, R, P, T <: Transmittables](implicit
    ev: Context.Base[S] <:< Context.Base[Message[Transmittable.Aux[B, I, R, P, T]]])
  : Endpoint[B, R]
}

object Context {
  sealed trait Base[S <: Transmittables] extends Context[S] {
    this: ContextBuilder.Context[S] =>

    def endpoint[B, I, R, P, T <: Transmittables](implicit
      ev: Base[S] <:< Base[Message[Transmittable.Aux[B, I, R, P, T]]])
    : Endpoint[B, R] =
      ev(this) match { case message: MessageEndpoint[B, I, R, P, T] =>
        message.endpoint
      }
  }

  trait MessageEndpoint[B, I, R, P, T <: Transmittables]
    extends Base[Message[Transmittable.Aux[B, I, R, P, T]]] {
    this: ContextBuilder.Context[Message[Transmittable.Aux[B, I, R, P, T]]] =>

    val endpoint: Endpoint[B, R]
  }

  trait DelegatesNoEndpoint[D <: Delegating] extends Base[Delegates[D]] {
    this: ContextBuilder.Context[Delegates[D]] =>
  }

  trait NoneNoEndpoint extends Base[None] {
    this: ContextBuilder.Context[None] =>
  }
}


trait SendingContext[S <: Transmittables] extends Context[S] {
  this: ContextBuilder.Context[S] =>

  def send[B, I, R, P, T <: Transmittables](
    value: B)(implicit selector: Selector[B, I, R, P, T, S]): I
}


trait ReceivingContext[S <: Transmittables] extends Context[S] {
  this: ContextBuilder.Context[S] =>

  def receive[B, I, R, P, T <: Transmittables](
    value: I)(implicit selector: Selector[B, I, R, P, T, S]): R
}
