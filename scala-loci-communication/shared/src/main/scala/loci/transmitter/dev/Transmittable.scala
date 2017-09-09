package loci
package transmitter
package dev

import scala.annotation.implicitNotFound
import scala.concurrent.Future


sealed trait NoDelegates extends Transmittable.Delegating

object NoDelegates extends NoDelegates


final case class /[
    R <: Transmittable.Delegating, T <: Transmittable.Messaging] private[dev] (
    tail: R, head: T) extends Transmittable.Delegating {
  type Type = R / T

  val tailDelegates = Transmittables.Delegates(tail)

  val headMessage = Transmittables.Message(head)

  def /[U](transmittable: Transmittable[U]): R / T / transmittable.Type =
    new / (this, transmittable)
}


sealed trait Transmittables

object Transmittables {
  final case class Delegates[T <: Transmittable.Delegating](delegates: T)
    extends Transmittables

  final case class Message[T <: Transmittable.Messaging](message: T)
    extends Transmittables
}


object Transmittable {
  type Aux[B, I, R, P, T <: Transmittables] = Transmittable[B] {
    type Intermediate = I
    type Result = R
    type Proxy = P
    type Transmittables = T
  }

  def apply[T](implicit transmittable: Transmittable[T]): transmittable.Type =
    transmittable

  sealed trait Delegating

  sealed trait Messaging { type Intermediate }
}


@implicitNotFound("${T} is not transmittable")
sealed trait Transmittable[T] extends
    Transmittable.Delegating with
    Transmittable.Messaging {
  type Base = T
  type Intermediate
  type Result
  type Proxy
  type Transmittables <: dev.Transmittables

  type Type = Transmittable[T] {
    type Intermediate = Transmittable.this.Intermediate
    type Result = Transmittable.this.Result
    type Proxy = Transmittable.this.Proxy
    type Transmittables = Transmittable.this.Transmittables
  }

  final def /[U](transmittable: Transmittable[U]): Type / transmittable.Type =
    new / (this, transmittable)

  val transmittables: Transmittables

  def send(value: Base)(
    implicit context: SendingContext[Transmittables]): Intermediate

  def receive(value: Intermediate)(
    implicit context: ReceivingContext[Transmittables]): Result
}


sealed trait DelegatingTransmittable[T] extends Transmittable[T] {
  type Proxy = Future[Result]
  type Transmittables = Transmittables.Delegates[Delegates]

  type Intermediate
  type Result
  type Delegates <: Transmittable.Delegating
}

object DelegatingTransmittable {
  type Delegates[D <: Transmittable.Delegating] = Transmittables.Delegates[D]

  class SendingContext[D <: Transmittable.Delegating](
      val context: dev.SendingContext[Delegates[D]]) {
    def send[B, I, R, P, T <: Transmittables](
        value: B)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): I =
      context send value
  }

  class ReceivingContext[D <: Transmittable.Delegating](
      val context: dev.ReceivingContext[Delegates[D]]) {
    def receive[B, I, R, P, T <: Transmittables](
        value: I)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): R =
      context receive value
  }

  def apply[B, I, R, D <: Transmittable.Delegating](
      delegates: D,
      send: (B, SendingContext[D]) => I,
      receive: (I, ReceivingContext[D]) => R) = {
    val _send = send
    val _receive = receive

    new DelegatingTransmittable[B] {
      type Intermediate = I
      type Result = R
      type Delegates = D

      val transmittables = Transmittables.Delegates(delegates)

      def send(value: Base)(
          implicit context: dev.SendingContext[Transmittables]) =
        _send(value, new DelegatingTransmittable.SendingContext(context))

      def receive(value: Intermediate)(
          implicit context: dev.ReceivingContext[Transmittables]) =
        _receive(value, new DelegatingTransmittable.ReceivingContext(context))
    }
  }
}


sealed trait ConnectedTransmittable[T] extends Transmittable[T] {
  type Intermediate = transmittables.message.Intermediate
  type Proxy = Future[Result]
  type Transmittables = Transmittables.Message[Message]

  type Result
  type Message <: Transmittable.Messaging
}

object ConnectedTransmittable {
  type Message[B, I, R, P, T <: Transmittables] =
    Transmittables.Message[Transmittable.Aux[B, I, R, P, T]]

  final class ConnectedContext[B, I, R, P, T <: Transmittables](
    val context: Context[Message[B, I, R, P, T]]) {
    val endpoint: Endpoint[B, R] = context.endpoint
  }

  def apply[B, R, B0, I0, R0, P0, T0 <: Transmittables](
      message: Transmittable.Aux[B0, I0, R0, P0, T0],
      send: (B, ConnectedContext[B0, I0, R0, P0, T0]) => B0,
      receive: (R0, ConnectedContext[B0, I0, R0, P0, T0]) => R) = {
    val _send = send
    val _receive = receive

    type T = Transmittable.Aux[B0, I0, R0, P0, T0]

    new ConnectedTransmittable[B] {
      type Result = R
      type Message = Transmittable.Aux[B0, I0, R0, P0, T0]

      val transmittables = Transmittables.Message(message)

      def send(value: Base)(
          implicit context: dev.SendingContext[Transmittables]) =
        context send _send(value, new ConnectedTransmittable.ConnectedContext(context))

      def receive(value: Intermediate)(
          implicit context: dev.ReceivingContext[Transmittables]) =
        _receive(context receive value, new ConnectedTransmittable.ConnectedContext(context))
    }
  }
}
