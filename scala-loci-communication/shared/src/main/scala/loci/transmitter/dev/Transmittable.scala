package loci
package transmitter
package dev

import scala.annotation.implicitNotFound


sealed trait Transmittables


sealed trait NoTransmittables extends Transmittables


object Message {
  type Transmittable = { type Base; type Intermediate; type Result }
}


sealed trait NoMessage { type Base; type Intermediate; type Result }


sealed trait /[R <: Transmittables, T <: Transmittable[_]]
  extends Transmittables


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
trait Transmittable[T] extends Transmittables {
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

  protected type Context =
    dev.Context[Transmittables, Message]
  protected type SendingContext =
    dev.SendingContext[Transmittables, Message]
  protected type ReceivingContext =
    dev.ReceivingContext[Transmittables, Message]

  protected implicit class TransmittableSendingOp[
      C <: Context, TB, TI, TR,
      TT <: dev.Transmittables, TM <: Message.Transmittable](
        transmittable: Transmittable.Aux[TB, TI, TR, TT, TM])(
      implicit
        context: C,
        sending: ContextType[C, SendingContext]) {
    def pass(v: TB)(
        implicit selector: Selector[transmittable.Type, Transmittables]): TI =
      context send (transmittable, v)
  }

  protected implicit class TransmittableReceivingOp[
      C <: Context, TB, TI, TR,
      TT <: dev.Transmittables, TM <: Message.Transmittable](
        transmittable: Transmittable.Aux[TB, TI, TR, TT, TM])(
      implicit
        context: C,
        receiving: ContextType[C, ReceivingContext]) {
    def pass(v: TI)(
        implicit selector: Selector[transmittable.Type, Transmittables]): TR =
      context receive (transmittable, v)
  }

  protected implicit class MessageEndpointOp(val transmittable: Message) {
    def endpoint[C <: Context](implicit context: C) =
      context.endpoint(transmittable)
  }

  def send(value: Base)(implicit context: SendingContext): Intermediate

  def receive(value: Intermediate)(implicit context: ReceivingContext): Result
}


trait DetachedTransmittable[T] extends Transmittable[T] {
  type Message = NoMessage
}


trait IsoTransmittable[T] extends DetachedTransmittable[T] {
  type Transmittables = NoTransmittables
}
