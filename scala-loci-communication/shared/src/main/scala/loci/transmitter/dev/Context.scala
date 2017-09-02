package loci
package transmitter
package dev

trait Context[T <: Transmittables] {
    this: ContextBuilder.Context[T] =>
  def endpoint[B0, I0, R0, T0 <: Transmittables](implicit
    ev: ContextBuilder.Equiv[T] <:<
        ContextBuilder.Equiv[Transmittables.Message[Transmittable.Aux[B0, I0, R0, T0]]])
  : Endpoint[B0, R0]
}

trait SendingContext[T <: Transmittables]
    extends Context[T] { this: ContextBuilder.Context[T] =>
  def send[B0, I0, R0, T0 <: Transmittables](
    value: B0)(implicit selector: Selector[B0, I0, R0, T0, T]): I0
}

trait ReceivingContext[T <: Transmittables]
    extends Context[T] { this: ContextBuilder.Context[T] =>
  def receive[B0, I0, R0, T0 <: Transmittables](
    value: I0)(implicit selector: Selector[B0, I0, R0, T0, T]): R0
}
