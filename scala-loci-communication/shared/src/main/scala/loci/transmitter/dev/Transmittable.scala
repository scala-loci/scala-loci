package loci
package transmitter
package dev

import scala.concurrent.Future


sealed trait NoDelegates extends Transmittable.Delegating

object NoDelegates extends NoDelegates


final case class /[
    D <: Transmittable.Delegating, T <: Transmittable[_, _, _]] private[dev] (
    tail: D, head: T) extends Transmittable.Delegating {
  type Type = D / T

  val tailDelegates = Transmittables.Delegates(tail)

  def /(transmittable: Transmittable[_, _, _]): D / T / transmittable.Type =
    new / (this, transmittable.common)
}


sealed trait Transmittables

object Transmittables {
  final case class Delegates[T <: Transmittable.Delegating] private[dev] (delegates: T)
    extends Transmittables

  final case class Message[T <: Transmittable.Messaging] private[dev] (message: T)
    extends Transmittables
}

sealed trait Transmittable[B, I, R] extends
    Transmittable.Base with
    Transmittable.Delegating with
    Transmittable.Messaging {
  type Base = B
  type Intermediate = I
  type Result = R
  type Proxy
  type Transmittables <: dev.Transmittables
  type Type = Transmittable.Common[Base, Intermediate, Result, Proxy, Transmittables]
}

object Transmittable {
  def apply[T](implicit resolution: Common.Resolution[T, _, _, _, _])
    : resolution.common.Type = resolution.common.common

  def Result[T](implicit resolution: Common.Resolution[_, _, T, _, _])
    : resolution.common.Type = resolution.common.common

  def Argument[T](implicit resolution: Common.Resolution[T, _, T, _, _])
    : resolution.common.Type = resolution.common.common

  sealed trait Delegating

  sealed trait Messaging

  sealed trait Base {
    type Type
    def common: Type
  }

  type Aux[B, I, R, P, T <: Transmittables] = Common[B, I, R, P, T]

  sealed trait Common[B, I, R, P, T <: Transmittables] extends Transmittable[B, I, R] {
    type Proxy = P
    type Transmittables = T

    final def common: Type = this

    val transmittables: Transmittables

    def send(value: Base)(
      implicit context: SendingContext[Transmittables]): Intermediate

    def receive(value: Intermediate)(
      implicit context: ReceivingContext[Transmittables]): Result
  }


  final case class SingletonValue[B, I, R, V] private[dev] (value: V) extends AnyVal

  object SingletonValue {
    implicit def singletonValue[B, I, R](implicit
      transmittable: Transmittable[B, I, R])
    : SingletonValue[B, I, R, transmittable.type] =
      SingletonValue(transmittable)
  }

  sealed trait CommonResolutionFallback {
    implicit def resolutionFallback[B, I, R, P, T <: Transmittables, V](implicit
      singleton: SingletonValue[B, I, R, V],
      parameters: V <:< Transmittable[B, I, R] { type Type = Common[B, I, R, P, T] })
    : Common.Resolution[B, I, R, P, T] =
      Common.Resolution(singleton.value.common)
  }

  object Common extends CommonResolutionFallback {
    final case class Resolution[B, I, R, P, T <: Transmittables] private[dev] (
      common: Common[B, I, R, P, T]) extends AnyVal

    implicit def resolution[
        B, I, R, P, T <: Transmittables, V <: Transmittable[B, I, R] {
          type Proxy = P
          type Transmittables = T
          type Type = Common[B, I, R, P, T]
        }](implicit
      singleton: SingletonValue[B, I, R, V])
    : Resolution[B, I, R, singleton.value.Proxy, singleton.value.Transmittables] =
      Resolution(singleton.value.common)
  }

  object Delegating {
    final case class Resolution[D <: Delegating] private[dev] (
      transmittables: D) extends AnyVal

    implicit def none[B, I, R, P, T <: Transmittables]
    : Resolution[NoDelegates] =
      Resolution(NoDelegates)

    implicit def single[B, I, R, P, T <: Transmittables](implicit
      resolution: Common.Resolution[B, I, R, P, T])
    : Resolution[Common[B, I, R, P, T]] =
      Resolution(resolution.common)

    implicit def list[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
      resolution: Common.Resolution[B, I, R, P, T],
      delegates: Resolution[D])
    : Resolution[D / Common[B, I, R, P, T]] =
      Resolution(/ (delegates.transmittables, resolution.common))
  }
}


sealed trait DelegatingTransmittable[Base, Intermediate, Result, Delegates <: Transmittable.Delegating] extends
    Transmittable.Common[Base, Intermediate, Result, Future[Result], Transmittables.Delegates[Delegates]]

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

    new DelegatingTransmittable[B, I, R, D] {
      val transmittables = Transmittables.Delegates(delegates.transmittables)

      def send(value: Base)(
          implicit context: dev.SendingContext[Transmittables]) =
        _send(value, new SendingContext)

      def receive(value: Intermediate)(
          implicit context: dev.ReceivingContext[Transmittables]) =
        _receive(value, new ReceivingContext)
    }
  }
}


sealed trait ConnectedTransmittable[Base, Intermediate, Result, Message <: Transmittable.Messaging] extends
  Transmittable.Common[Base, Intermediate, Result, Future[Result], Transmittables.Message[Message]]

object ConnectedTransmittable {
  type Message[B, I, R, P, T <: Transmittables] =
    Transmittables.Message[Transmittable.Aux[B, I, R, P, T]]

  final class ConnectedContext[B, I, R, P, T <: Transmittables](
      implicit context: Context[Message[B, I, R, P, T]]) {
    val endpoint: Endpoint[B, R] = context.endpoint
  }

  def apply[B, R, B0, I0, R0, P0, T0 <: Transmittables](
      send: (B, ConnectedContext[B0, I0, R0, P0, T0]) => B0,
      receive: (R0, ConnectedContext[B0, I0, R0, P0, T0]) => R)(
    implicit
      message: Transmittable.Common.Resolution[B0, I0, R0, P0, T0]) = {
    val _send = send
    val _receive = receive

    new ConnectedTransmittable[B, I0, R, Transmittable.Common[B0, I0, R0, P0, T0]] {
      val transmittables = Transmittables.Message(message.common)

      def send(value: Base)(
          implicit context: SendingContext[Transmittables]) =
        context send _send(value, new ConnectedContext)

      def receive(value: Intermediate)(
          implicit context: ReceivingContext[Transmittables]) =
        _receive(context receive value, new ConnectedContext)
    }
  }
}
