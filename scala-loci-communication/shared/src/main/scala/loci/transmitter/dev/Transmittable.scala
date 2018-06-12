package loci
package transmitter
package dev

import scala.concurrent.Future


final class /[D <: Transmittable.Delegating, T <: Transmittable[_, _, _]] private[dev] (
    val tail: D, val head: T) extends Transmittable.Delegating {
  type Type = D / T

  def tailDelegates = new Transmittables.Delegates(tail)

  def /[B, I, R](transmittable: Transmittable[B, I, R]): D / T / transmittable.Type =
    new / (this, transmittable)
}


sealed trait Transmittables extends Any

object Transmittables {
  final class Delegates[T <: Transmittable.Delegating] private[dev] (val delegates: T)
    extends AnyVal with Transmittables

  final class Message[T <: Transmittable[_, _, _]] private[dev] (val message: T)
    extends AnyVal with Transmittables

  final class None private[dev] extends Transmittables
}

sealed trait Transmittable[B, I, R] extends Transmittable.Delegating {
  type Base = B
  type Intermediate = I
  type Result = R
  type Proxy
  type Transmittables <: dev.Transmittables
  type Type = Transmittable.Aux[Base, Intermediate, Result, Proxy, Transmittables]

  val transmittables: Transmittables

  def send(value: Base)(
    implicit context: SendingContext[Transmittables]): Intermediate

  def receive(value: Intermediate)(
    implicit context: ReceivingContext[Transmittables]): Result
}

object Transmittable {
  sealed trait Delegating

  type Aux[B, I, R, P, T <: Transmittables] = Transmittable[B, I, R] {
    type Proxy = P
    type Transmittables = T
  }


  def apply[T](implicit resolution: Aux.Resolution[T, _, _, _, _])
    : resolution.transmittable.Type = resolution.transmittable

  def Argument[T](implicit resolution: Aux.Resolution[T, _, T, _, _])
    : resolution.transmittable.Type = resolution.transmittable


  final class SingletonValue[B, I, R, V] private (val value: V) extends AnyVal

  object SingletonValue {
    implicit def singletonValue[B, I, R](implicit
      transmittable: Transmittable[B, I, R])
    : SingletonValue[B, I, R, transmittable.type] =
      new SingletonValue(transmittable)
  }

  sealed trait AuxResolutionFallback {
    implicit def resolutionFallback[B, I, R, P, T <: Transmittables, V](implicit
      singleton: SingletonValue[B, I, R, V],
      parameters: V <:< Transmittable[B, I, R] {
        type Proxy = P
        type Transmittables = T
        type Type = Aux[B, I, R, P, T]
      })
    : Aux.Resolution[B, I, R, P, T] =
      new Aux.Resolution(singleton.value)
  }

  object Aux extends AuxResolutionFallback {
    final class Resolution[B, I, R, P, T <: Transmittables] private[Transmittable] (
      val transmittable: Aux[B, I, R, P, T]) extends AnyVal

    implicit def resolution[
        B, I, R, P, T <: Transmittables, V <: Transmittable[B, I, R] {
          type Proxy = P
          type Transmittables = T
          type Type = Aux[B, I, R, P, T]
        }](implicit
      singleton: SingletonValue[B, I, R, V])
    : Resolution[B, I, R, singleton.value.Proxy, singleton.value.Transmittables] =
      new Resolution(singleton.value)
  }

  object Delegating {
    final class Resolution[D <: Delegating] private[Transmittable] (
      val transmittables: D) extends AnyVal

    implicit def single[B, I, R, P, T <: Transmittables](implicit
      resolution: Aux.Resolution[B, I, R, P, T])
    : Resolution[Aux[B, I, R, P, T]] =
      new Resolution(resolution.transmittable)

    implicit def list[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
      resolution: Aux.Resolution[B, I, R, P, T],
      delegates: Resolution[D])
    : Resolution[D / Aux[B, I, R, P, T]] =
      new Resolution(new / (delegates.transmittables, resolution.transmittable))
  }
}


sealed trait DelegatingTransmittable[B, I, R] extends Transmittable[B, I, R] {
  type Proxy = Future[R]
  type Transmittables = Transmittables.Delegates[Delegates]
  type Delegates <: Transmittable.Delegating
}

object DelegatingTransmittable {
  type Delegates[D <: Transmittable.Delegating] = Transmittables.Delegates[D]

  class SendingContext[D <: Transmittable.Delegating](
      implicit context: dev.SendingContext[Delegates[D]]) {
    def send[B, I, R, P, T <: Transmittables](
        value: B)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): I =
      context send value
  }

  class ReceivingContext[D <: Transmittable.Delegating](
      implicit context: dev.ReceivingContext[Delegates[D]]) {
    def receive[B, I, R, P, T <: Transmittables](
        value: I)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): R =
      context receive value
  }

  def apply[B, I, R, D <: Transmittable.Delegating](
      send: (B, SendingContext[D]) => I,
      receive: (I, ReceivingContext[D]) => R)(
    implicit
      delegates: Transmittable.Delegating.Resolution[D]) = {
    val _send = send
    val _receive = receive

    new DelegatingTransmittable[B, I, R] {
      type Delegates = D

      val transmittables = new Transmittables.Delegates(delegates.transmittables)

      def send(value: Base)(
          implicit context: dev.SendingContext[Transmittables]) =
        _send(value, new SendingContext)

      def receive(value: Intermediate)(
          implicit context: dev.ReceivingContext[Transmittables]) =
        _receive(value, new ReceivingContext)
    }
  }
}


sealed trait ConnectedTransmittable[B, I, R] extends Transmittable[B, I, R] {
  type Proxy = Future[R]
  type Transmittables = Transmittables.Message[Message]
  type Message <: Transmittable[_, _, _]
}

object ConnectedTransmittable {
  final class ConnectedContext[B, I, R, P, T <: Transmittables](implicit
      context: Context[Transmittables.Message[Transmittable.Aux[B, I, R, P, T]]]) {
    val endpoint: Endpoint[B, R] = context.endpoint
  }

  def apply[B, R, B0, I0, R0, P0, T0 <: Transmittables](
      send: (B, ConnectedContext[B0, I0, R0, P0, T0]) => B0,
      receive: (R0, ConnectedContext[B0, I0, R0, P0, T0]) => R)(
    implicit
      message: Transmittable.Aux.Resolution[B0, I0, R0, P0, T0]) = {
    val _send = send
    val _receive = receive

    new ConnectedTransmittable[B, I0, R] {
      type Message = Transmittable.Aux[B0, I0, R0, P0, T0]

      val transmittables = new Transmittables.Message(message.transmittable)

      def send(value: Base)(
          implicit context: SendingContext[Transmittables]) =
        context send _send(value, new ConnectedContext)

      def receive(value: Intermediate)(
          implicit context: ReceivingContext[Transmittables]) =
        _receive(context receive value, new ConnectedContext)
    }
  }
}
