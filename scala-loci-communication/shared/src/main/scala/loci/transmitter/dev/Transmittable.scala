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

  private sealed trait TransmittableOp[A, B] {
    def apply(value: A): B
  }

  private object TransmittableOp {
    implicit def sending[
      B0, I0, R0, T0 <: dev.Transmittables, M0 <: Message.Transmittable,
      C <: Context](implicit
        context: C,
        sending: ContextType[C, SendingContext],
        selector: Selector[B0, I0, R0, T0, M0, Transmittables]) =
      new TransmittableOp[B0, I0] {
        def apply(value: B0) = context send (transmittables, value)
      }

    implicit def receiving[
      B0, I0, R0, T0 <: dev.Transmittables, M0 <: Message.Transmittable,
      C <: Context](implicit
        context: C,
        receiving: ContextType[C, ReceivingContext],
        selector: Selector[B0, I0, R0, T0, M0, Transmittables]) =
      new TransmittableOp[I0, R0] {
        def apply(value: I0) = context receive (transmittables, value)
      }
  }

  final protected def process[A, B](value: A)(implicit op: TransmittableOp[A, B]) =
    op(value)

  final protected def endpoint[
    B0, I0, R0, T0 <: dev.Transmittables, M0 <: Message.Transmittable,
    C <: Context](implicit
      context: Context,
      ev0: Message <:< Transmittable.Aux[B0, I0, R0, T0, M0],
      ev1: dev.Context[Transmittables, Message] <:<
           dev.Context[Transmittables, Transmittable.Aux[B0, I0, R0, T0, M0]]) =
    ev1(context) endpoint ev0(message)

  def send(value: Base)(implicit context: SendingContext): Intermediate

  def receive(value: Intermediate)(implicit context: ReceivingContext): Result
}


trait DelegatingTransmittable[B, I, R, T <: Transmittables]
    extends Transmittable[B] {
  type Intermediate = I
  type Result = R
  type Transmittables = T
  type Message = NoMessage

  final def message = NoMessage
}

trait ConnectedTransmittable[B, I, R, T <: Transmittable[_]]
    extends Transmittable[B] {
  type Intermediate = I
  type Result = R
  type Transmittables = T
  type Message = T

  final def transmittables = message
}
