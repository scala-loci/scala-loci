package loci
package transmitter

import loci.contexts.Immediate.Implicits.global

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.concurrent.Future


final class /[D <: Transmittable.Delegating, T <: Transmittable[_, _, _]](
    val tail: D, val head: T) extends Transmittable.Delegating {
  type Type = D / T

  def tailDelegates = new Transmittables.Delegates(tail)

  def /[B, I, R](transmittable: Transmittable[B, I, R]): D / T / transmittable.Type =
    new / (this, transmittable)
}


sealed trait Transmittables extends Any

object Transmittables {
  final class Delegates[T <: Transmittable.Delegating](val delegates: T)
    extends AnyVal with Transmittables

  final class Message[T <: Transmittable[_, _, _]](val message: T)
    extends AnyVal with Transmittables

  final class None extends Transmittables
}


@implicitNotFound("${B} is not transmittable")
sealed trait Transmittable[-B, I, +R] extends Transmittable.Delegating {
  type Base = B @uncheckedVariance
  type Intermediate = I
  type Result = R @uncheckedVariance
  type Proxy
  type Transmittables <: transmitter.Transmittables
  type Type = Transmittable.Aux[Base, Intermediate, Result, Proxy, Transmittables] @uncheckedVariance

  val transmittables: Transmittables

  @inline final def self: Type = this

  def buildIntermediate(value: Base)(
    implicit context: Context.Providing[Transmittables]): Intermediate

  def buildResult(value: Intermediate)(
    implicit context: Context.Receiving[Transmittables]): Result

  def buildProxy(value: Future[Intermediate])(
    implicit context: Context.Receiving[Transmittables]): Proxy
}

object Transmittable extends
    TransmittablePrimitives with
    TransmittableTuples with
    TransmittableCollections {

  sealed trait Delegating

  type Aux[-B, I, +R, P, T <: Transmittables] = Transmittable[B, I, R] {
    type Proxy = P
    type Transmittables = T
  }


  @inline def apply[T](implicit resolution: Aux.Resolution[T, _, _, _, _])
  : resolution.transmittable.Type = resolution.transmittable.self

  @inline def Argument[T](implicit resolution: Aux.Resolution[T, _, T, _, _])
  : resolution.transmittable.Type = resolution.transmittable.self


  final class SingletonValue[B, I, R, +V] private (val value: V) extends AnyVal

  object SingletonValue {
    implicit def singletonValue[B, I, R](implicit
      transmittable: Transmittable[B, I, R])
    : SingletonValue[B, I, R, transmittable.type] =
      new SingletonValue(transmittable)
  }

  object Aux {
    @implicitNotFound("${B} is not transmittable")
    final class Resolution[B, I, R, P, T <: Transmittables] private[Aux] (
      val transmittable: Aux[B, I, R, P, T]) extends AnyVal

    sealed trait ResolutionFallback {
      @compileTimeOnly("Cannot resolve Transmittable instance")
      implicit def resolution[B, I, R, P, T <: Transmittables]
      : Resolution[B, I, R, P, T] =
        throw new NotImplementedError
    }

    object Resolution extends ResolutionFallback {
      implicit def resolution[B, I, R, P, T <: Transmittables](implicit
        singleton: SingletonValue[B, I, R, Aux[B, I, R, P, T]])
      : Resolution[B, I, R, P, T] =
        new Resolution(singleton.value)
    }

    implicit def resolution[B, I, R](implicit
      transmittable: Transmittable[B, I, R])
    : Resolution[B, I, R, transmittable.Proxy, transmittable.Transmittables] =
      new Resolution(transmittable)
  }

  object Delegating {
    final class Resolution[D <: Delegating] private[Delegating] (
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


sealed trait IdenticallyTransmittable[B] extends Transmittable[B, B, B] {
  type Proxy = Future[B]
  type Transmittables = Transmittables.None
}

object IdenticallyTransmittable {
  @inline def apply[B](): IdenticallyTransmittable[B] = implementation: Impl[B]

  sealed trait Impl[-B] extends IdenticallyTransmittable[B @uncheckedVariance]

  val implementation = new Impl[Any] {
    val transmittables = new Transmittables.None

    def buildIntermediate(value: Base)(
      implicit context: Context.Providing[Transmittables]) = value

    def buildResult(value: Intermediate)(
      implicit context: Context.Receiving[Transmittables]) = value

    def buildProxy(value: Future[Intermediate])(
      implicit context: Context.Receiving[Transmittables]) = value
  }
}


sealed trait TransformingTransmittable[B, I, R] extends Transmittable[B, I, R] {
  type Proxy = Future[R]
  type Transmittables = Transmittables.None
}

object TransformingTransmittable {
  final class Context private[TransformingTransmittable] (val remote: RemoteRef)

  def apply[B, I, R](
      provide: (B, Context) => I,
      receive: (I, Context) => R) =
    new TransformingTransmittable[B, I, R] {
      val transmittables = new Transmittables.None

      def buildIntermediate(value: Base)(
          implicit context: Context.Providing[Transmittables]) =
        provide(value, new Context(context.remote))

      def buildResult(value: Intermediate)(
          implicit context: Context.Receiving[Transmittables]) =
        receive(value, new Context(context.remote))

      def buildProxy(value: Future[Intermediate])(
          implicit context: Context.Receiving[Transmittables]) =
        value map buildResult
    }
}


sealed trait DelegatingTransmittable[B, I, R] extends Transmittable[B, I, R] {
  type Proxy = Future[R]
  type Transmittables = Transmittables.Delegates[Delegates]
  type Delegates <: Transmittable.Delegating
}

object DelegatingTransmittable {
  type Delegates[D <: Transmittable.Delegating] = Transmittables.Delegates[D]

  final class ProvidingContext[D <: Transmittable.Delegating] private[DelegatingTransmittable] (
      implicit context: Context.Providing[Delegates[D]]) {
    val remote = context.remote
    def delegate[B, I, R, P, T <: Transmittables](
        value: B)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): I =
      context provide value
  }

  final class ReceivingContext[D <: Transmittable.Delegating] private[DelegatingTransmittable] (
      implicit context: Context.Receiving[Delegates[D]]) {
    val remote = context.remote
    def delegate[B, I, R, P, T <: Transmittables](
        value: I)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): R =
      context receive value
  }

  def apply[B, I, R, D <: Transmittable.Delegating](
      provide: (B, ProvidingContext[D]) => I,
      receive: (I, ReceivingContext[D]) => R)(
    implicit
      delegates: Transmittable.Delegating.Resolution[D]) =
    new DelegatingTransmittable[B, I, R] {
      type Delegates = D

      val transmittables = new Transmittables.Delegates(delegates.transmittables)

      def buildIntermediate(value: Base)(
          implicit context: Context.Providing[Transmittables]) =
        provide(value, new ProvidingContext)

      def buildResult(value: Intermediate)(
          implicit context: Context.Receiving[Transmittables]) =
        receive(value, new ReceivingContext)

      def buildProxy(value: Future[Intermediate])(
          implicit context: Context.Receiving[Transmittables]) =
        value map buildResult
    }
}


sealed trait ConnectedTransmittable[B, I, R] extends Transmittable[B, I, R] {
  type Proxy = Future[R]
  type Transmittables = Transmittables.Message[Message]
  type Message <: Transmittable[_, _, _]
}

object ConnectedTransmittable {
  final class Context[B, I, R, P, T <: Transmittables] private[ConnectedTransmittable] (implicit
      context: transmitter.Context[Transmittables.Message[Transmittable.Aux[B, I, R, P, T]]]) {
    val remote = context.remote
    val endpoint: Endpoint[B, R] = context.endpoint
  }

  def apply[B, R, B0, I0, R0, P0, T0 <: Transmittables](
      provide: (B, Context[B0, I0, R0, P0, T0]) => B0,
      receive: (R0, Context[B0, I0, R0, P0, T0]) => R)(
    implicit
      message: Transmittable.Aux.Resolution[B0, I0, R0, P0, T0]) =
    new ConnectedTransmittable[B, I0, R] {
      type Message = Transmittable.Aux[B0, I0, R0, P0, T0]

      val transmittables = new Transmittables.Message(message.transmittable)

      def buildIntermediate(value: Base)(
          implicit context: Context.Providing[Transmittables]) =
        context provide provide(value, new Context)

      def buildResult(value: Intermediate)(
          implicit context: Context.Receiving[Transmittables]) =
        receive(context receive value, new Context)

      def buildProxy(value: Future[Intermediate])(
          implicit context: Context.Receiving[Transmittables]) =
        value map buildResult
    }


  sealed trait Proxy[B, I, R] extends Transmittable[B, I, R] {
    type Transmittables = Transmittables.Message[Message]
    type Message <: Transmittable[_, _, _]
    type Internal
  }

  object Proxy {
    def apply[B, R, P, N, B0, I0, R0, P0, T0 <: Transmittables](
        provide: (B, Context[B0, I0, R0, P0, T0]) => B0,
        receive: (R0, Context[B0, I0, R0, P0, T0]) => N,
        direct: (N, Context[B0, I0, R0, P0, T0]) => R,
        proxy: (Future[N], Context[B0, I0, R0, P0, T0]) => P)(
      implicit
        message: Transmittable.Aux.Resolution[B0, I0, R0, P0, T0]) =
      new Proxy[B, I0, R] {
        type Message = Transmittable.Aux[B0, I0, R0, P0, T0]
        type Internal = N
        type Proxy = P

        val transmittables = new Transmittables.Message(message.transmittable)

        def buildIntermediate(value: Base)(
            implicit context: Context.Providing[Transmittables]) =
          context provide provide(value, new Context)

        def buildResult(value: Intermediate)(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          direct(receive(context receive value, ctx), ctx)
        }

        def buildProxy(value: Future[Intermediate])(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          proxy(value map { value => receive(context receive value, ctx) },  ctx)
        }
      }

    def apply[B, R, P, N, B0, I0, R0, P0, T0 <: Transmittables](
        internal: => N,
        provide: (B, Context[B0, I0, R0, P0, T0]) => B0,
        receive: (N, R0, Context[B0, I0, R0, P0, T0]) => Unit,
        direct: (N, Context[B0, I0, R0, P0, T0]) => R,
        proxy: (N, Context[B0, I0, R0, P0, T0]) => P)(
      implicit
        message: Transmittable.Aux.Resolution[B0, I0, R0, P0, T0]) =
      new Proxy[B, I0, R] {
        type Message = Transmittable.Aux[B0, I0, R0, P0, T0]
        type Internal = N
        type Proxy = P

        val transmittables = new Transmittables.Message(message.transmittable)

        def buildIntermediate(value: Base)(
            implicit context: Context.Providing[Transmittables]) =
          context provide provide(value, new Context)

        def buildResult(value: Intermediate)(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          val inst = internal
          receive(inst, context receive value, ctx)
          direct(inst, ctx)
        }

        def buildProxy(value: Future[Intermediate])(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          val inst = internal
          value foreach { value => receive(inst, context receive value, ctx) }
          proxy(inst, ctx)
        }
      }
  }
}
