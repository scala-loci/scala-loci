package loci
package transmitter
package dev

import scala.annotation.implicitNotFound


sealed trait Transmittables


sealed trait NoTransmittables extends Transmittables

object NoTransmittables extends NoTransmittables


object Message {
  type Transmittable = { type Base; type Intermediate; type Result }
}


sealed trait NoMessage { type Base; type Intermediate; type Result }

object NoMessage extends NoMessage


final case class /[R <: Transmittables, T <: Transmittable[_]] private[dev] (
    rest: R, transmittable: T) extends Transmittables {
  type Type = R / T
  def /[U](transmittable: Transmittable[U]): R / T / transmittable.Type =
    new / (this, transmittable)
}


object Transmittable {
  type Aux[
    B, I, R, T <: transmitter.dev.Transmittables, M <: Message.Transmittable] =
      Transmittable[B] {
        type Intermediate = I
        type Result = R
        type Transmittables = T
        type Message = M
      }

  def apply[T](implicit transmittable: Transmittable[T]): transmittable.Type =
    transmittable

  trait SendingContext[S <: Transmittables] {
    val context: dev.SendingContext[S, _]
    def send[
        B, I, R, T <: dev.Transmittables, M <: Message.Transmittable](
        value: B)(implicit selector: Selector[B, I, R, T, M, S]): I =
      context send value
  }

  trait ReceivingContext[S <: Transmittables] {
    val context: dev.ReceivingContext[S, _]
    def receive[
        B, I, R, T <: dev.Transmittables, M <: Message.Transmittable](
        value: I)(implicit selector: Selector[B, I, R, T, M, S]): R =
      context receive value
  }

  trait ConnectedContext[S <: Transmittable.Aux[B, _, R, _, _], B, R] {
    val context: dev.Context[S, S]
    val message: S
    def endpoint: Endpoint[B, R] = context endpoint message
  }
}


@implicitNotFound("${T} is not transmittable")
sealed trait Transmittable[T] extends Transmittables {
  type Base = T
  type Intermediate
  type Result
  type Transmittables <: dev.Transmittables
  type Message <: Message.Transmittable

  type Type = Transmittable[T] {
    type Intermediate = Transmittable.this.Intermediate
    type Result = Transmittable.this.Result
    type Transmittables = Transmittable.this.Transmittables
    type Message = Transmittable.this.Message
  }

  def /[U](transmittable: Transmittable[U]): Type / transmittable.Type =
    new / (this, transmittable)

  def transmittables: Transmittables

  def message: Message

  protected type Context =
    dev.Context[Transmittables, Message]
  protected type SendingContext =
    dev.SendingContext[Transmittables, Message]
  protected type ReceivingContext =
    dev.ReceivingContext[Transmittables, Message]

  def send(value: Base)(implicit context: SendingContext): Intermediate

  def receive(value: Intermediate)(implicit context: ReceivingContext): Result
}


sealed trait DelegatingTransmittable[B, I, R, T <: Transmittables]
    extends Transmittable[B] {
  type Intermediate = I
  type Result = R
  type Transmittables = T
  type Message = NoMessage

  final def message = NoMessage
}

object DelegatingTransmittable {
  class SendingContext[S <: Transmittables](
      val context: dev.SendingContext[S, _]) extends
    Transmittable.SendingContext[S]

  class ReceivingContext[S <: Transmittables](
      val context: dev.ReceivingContext[S, _]) extends
    Transmittable.ReceivingContext[S]

  def apply[B, I, R, T <: Transmittables](
      transmittables: T,
      send: (B, SendingContext[T]) => I,
      receive: (I, ReceivingContext[T]) => R) = {
    val _transmittables = transmittables
    val _send = send
    val _receive = receive

    new DelegatingTransmittable[B, I, R, T] {
      val transmittables = _transmittables

      def send(value: Base)(implicit context: SendingContext) = {
        _send(value, new DelegatingTransmittable.SendingContext(context))
      }

      def receive(value: Intermediate)(implicit context: ReceivingContext) = {
        _receive(value, new DelegatingTransmittable.ReceivingContext(context))
      }
    }
  }
}


sealed trait ConnectedTransmittable[B, I, R, T <: Transmittable[_]]
    extends Transmittable[B] {
  type Intermediate = I
  type Result = R
  type Transmittables = T
  type Message = T

  final def transmittables = message
}

object ConnectedTransmittable {
  class SendingContext[S <: Transmittable.Aux[SB, _, SR, _, _], SB, SR](
      val context: dev.SendingContext[S, S],
      val message: S) extends
    Transmittable.SendingContext[S] with
    Transmittable.ConnectedContext[S, SB, SR]

  class ReceivingContext[S <: Transmittable.Aux[SB, _, SR, _, _], SB, SR](
      val context: dev.ReceivingContext[S, S],
      val message: S) extends
    Transmittable.ReceivingContext[S] with
    Transmittable.ConnectedContext[S, SB, SR]

  def apply[B, I, R, TB, TI, TR, TT <: dev.Transmittables, TM <: Message.Transmittable](
      message: Transmittable.Aux[TB, TI, TR, TT, TM],
      send: (B, SendingContext[Transmittable.Aux[TB, TI, TR, TT, TM], TB, TR]) => I,
      receive: (I, ReceivingContext[Transmittable.Aux[TB, TI, TR, TT, TM], TB, TR]) => R) = {
    val _message = message
    val _send = send
    val _receive = receive

    type T = Transmittable.Aux[TB, TI, TR, TT, TM]

    new ConnectedTransmittable[B, I, R, Transmittable.Aux[TB, TI, TR, TT, TM]] {
      val message = _message

      def send(value: Base)(implicit context: SendingContext) = {
        _send(value, new ConnectedTransmittable.SendingContext(context, message))
      }

      def receive(value: Intermediate)(implicit context: ReceivingContext) = {
        _receive(value, new ConnectedTransmittable.ReceivingContext(context, message))
      }
    }
  }
}
