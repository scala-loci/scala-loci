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
  def send[B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
      transmittable: Transmittable.Aux[B0, I0, R0, T0, M0], v: B0)(
    implicit
      selector: Selector[B0, I0, R0, T0, M0, T])
    : transmittable.Intermediate
}

trait ReceivingContext[T <: Transmittables, M <: Message.Transmittable]
    extends Context[T, M] { this: ContextBuilder.Context[T, M] =>
  def receive[B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
      transmittable: Transmittable.Aux[B0, I0, R0, T0, M0], v: I0)(
    implicit
      selector: Selector[B0, I0, R0, T0, M0, T])
    : transmittable.Result
}
