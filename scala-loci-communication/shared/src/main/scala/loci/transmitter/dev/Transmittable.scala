package loci
package transmitter
package dev

import scala.annotation.implicitNotFound


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
  type Aux[B, I, R, T <: Transmittables] = Transmittable[B] {
    type Intermediate = I
    type Result = R
    type Transmittables = T
  }

  def apply[T](implicit transmittable: Transmittable[T]): transmittable.Type =
    transmittable


  sealed trait Delegating

  sealed trait Messaging


  sealed trait SendingContext[S <: Transmittables] {
    val context: dev.SendingContext[S]
    def send[B, I, R, T <: dev.Transmittables](
        value: B)(implicit selector: Selector[B, I, R, T, S]): I =
      context send value
  }

  sealed trait ReceivingContext[S <: Transmittables] {
    val context: dev.ReceivingContext[S]
    def receive[B, I, R, T <: dev.Transmittables](
        value: I)(implicit selector: Selector[B, I, R, T, S]): R =
      context receive value
  }

  sealed trait ConnectedContext[
      B, I, R, T <: Transmittables,
      S <: Transmittables.Message[Transmittable.Aux[B, I, R, T]]] {
    val context: dev.Context[S]
    val endpoint: Endpoint[B, R] = context.endpoint
  }
}


@implicitNotFound("${T} is not transmittable")
sealed trait Transmittable[T] extends
    Transmittable.Delegating with
    Transmittable.Messaging {
  type Base = T
  type Intermediate
  type Result
  type Transmittables <: dev.Transmittables

  type Type = Transmittable[T] {
    type Intermediate = Transmittable.this.Intermediate
    type Result = Transmittable.this.Result
    type Transmittables = Transmittable.this.Transmittables
  }

  final def /[U](transmittable: Transmittable[U]): Type / transmittable.Type =
    new / (this, transmittable)

  def transmittables: Transmittables

  protected type Context =
    dev.Context[Transmittables]
  protected type SendingContext =
    dev.SendingContext[Transmittables]
  protected type ReceivingContext =
    dev.ReceivingContext[Transmittables]

  def send(value: Base)(implicit context: SendingContext): Intermediate

  def receive(value: Intermediate)(implicit context: ReceivingContext): Result
}


sealed trait DelegatingTransmittable[B, I, R, T <: Transmittable.Delegating]
    extends Transmittable[B] {
  type Intermediate = I
  type Result = R
  type Transmittables = Transmittables.Delegates[T]
}

object DelegatingTransmittable {
  type Delegates[T <: Transmittable.Delegating] =
    Transmittables.Delegates[T]

  class SendingContext[T <: Transmittable.Delegating](
      val context: dev.SendingContext[Delegates[T]]) extends
    Transmittable.SendingContext[Delegates[T]]

  class ReceivingContext[T <: Transmittable.Delegating](
      val context: dev.ReceivingContext[Delegates[T]]) extends
    Transmittable.ReceivingContext[Delegates[T]]

  def apply[B, I, R, T <: Transmittable.Delegating](
      delegates: T,
      send: (B, SendingContext[T]) => I,
      receive: (I, ReceivingContext[T]) => R) = {
    val _send = send
    val _receive = receive

    new DelegatingTransmittable[B, I, R, T] {
      val transmittables = Transmittables.Delegates(delegates)

      def send(value: Base)(implicit context: SendingContext) = {
        _send(value, new DelegatingTransmittable.SendingContext(context))
      }

      def receive(value: Intermediate)(implicit context: ReceivingContext) = {
        _receive(value, new DelegatingTransmittable.ReceivingContext(context))
      }
    }
  }
}


sealed trait ConnectedTransmittable[B, I, R, T <: Transmittable.Messaging]
    extends Transmittable[B] {
  type Intermediate = I
  type Result = R
  type Transmittables = Transmittables.Message[T]
}

object ConnectedTransmittable {
  type Message[B, I, R, T <: Transmittables] =
    Transmittables.Message[Transmittable.Aux[B, I, R, T]]

  class SendingContext[B, I, R, T <: Transmittables](
      val context: dev.SendingContext[Message[B, I, R, T]]) extends
    Transmittable.SendingContext[Message[B, I, R, T]] with
    Transmittable.ConnectedContext[B, I, R, T, Message[B, I, R, T]]

  class ReceivingContext[B, I, R, T <: Transmittables](
      val context: dev.ReceivingContext[Message[B, I, R, T]]) extends
    Transmittable.ReceivingContext[Message[B, I, R, T]] with
    Transmittable.ConnectedContext[B, I, R, T, Message[B, I, R, T]]

  def apply[B, I, R, B0, I0, R0, T0 <: dev.Transmittables](
      message: Transmittable.Aux[B0, I0, R0, T0],
      send: (B, SendingContext[B0, I0, R0, T0]) => I,
      receive: (I, ReceivingContext[B0, I0, R0, T0]) => R) = {
    val _send = send
    val _receive = receive

    type T = Transmittable.Aux[B0, I0, R0, T0]

    new ConnectedTransmittable[B, I, R, Transmittable.Aux[B0, I0, R0, T0]] {
      val transmittables = Transmittables.Message(message)

      def send(value: Base)(implicit context: SendingContext) = {
        _send(value, new ConnectedTransmittable.SendingContext(context))
      }

      def receive(value: Intermediate)(implicit context: ReceivingContext) = {
        _receive(value, new ConnectedTransmittable.ReceivingContext(context))
      }
    }
  }
}
