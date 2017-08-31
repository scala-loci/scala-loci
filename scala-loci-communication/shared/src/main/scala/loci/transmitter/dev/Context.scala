package loci
package transmitter
package dev

trait Context[T <: Transmittables, M <: Message.Transmittable] {
    this: ContextBuilder.Context[T, M] =>
  def endpoint(implicit transmittable: M)
    : Endpoint[transmittable.Base, transmittable.Result]
}

trait SendingContext[T <: Transmittables, M <: Message.Transmittable]
    extends Context[T, M] { this: ContextBuilder.Context[T, M] =>
  def send[U, V](transmittable: Transmittable[U], v: V)(
    implicit
      ev: V <:< transmittable.Base,
      selector: Selector[transmittable.Type, T])
    : transmittable.Intermediate
}

trait ReceivingContext[T <: Transmittables, M <: Message.Transmittable]
    extends Context[T, M] { this: ContextBuilder.Context[T, M] =>
  def receive[U, V](transmittable: Transmittable[U], v: V)(
    implicit
      ev: V <:< transmittable.Intermediate,
      selector: Selector[transmittable.Type, T])
    : transmittable.Result
}
