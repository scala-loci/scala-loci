package loci
package transmitter
package dev

trait Context[T <: Transmittables, M <: Message.Transmittable] {
    this: ContextBuilder.Context[T, M] =>
  def endpoint(transmittable: M)(implicit ev: M <:< Transmittable[_])
    : Endpoint[transmittable.Base, transmittable.Result]
}

trait SendingContext[T <: Transmittables, M <: Message.Transmittable]
    extends Context[T, M] { this: ContextBuilder.Context[T, M] =>
  def send[
    B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
    value: B0)(implicit selector: Selector[B0, I0, R0, T0, M0, T]): I0
}

trait ReceivingContext[T <: Transmittables, M <: Message.Transmittable]
    extends Context[T, M] { this: ContextBuilder.Context[T, M] =>
  def receive[
    B0, I0, R0, T0 <: Transmittables, M0 <: Message.Transmittable](
    value: I0)(implicit selector: Selector[B0, I0, R0, T0, M0, T]): R0
}
