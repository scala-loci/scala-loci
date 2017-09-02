package loci
package transmitter
package dev

trait Context[S <: Transmittables] {
    this: ContextBuilder.Context[S] =>
  def endpoint[B, I, R, T <: Transmittables](implicit
    ev: ContextBuilder.Equiv[S] <:<
        ContextBuilder.Equiv[Transmittables.Message[Transmittable.Aux[B, I, R, T]]])
  : Endpoint[B, R]
}

trait SendingContext[S <: Transmittables]
    extends Context[S] { this: ContextBuilder.Context[S] =>
  def send[B, I, R, T <: Transmittables](
    value: B)(implicit selector: Selector[B, I, R, T, S]): I
}

trait ReceivingContext[S <: Transmittables]
    extends Context[S] { this: ContextBuilder.Context[S] =>
  def receive[B, I, R, T <: Transmittables](
    value: I)(implicit selector: Selector[B, I, R, T, S]): R
}
