package loci
package transmitter

import loci.contexts.Immediate.Implicits.global

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object RemoteAccessor {
  trait Default { this: language.PlacedValue.type =>
    implicit class BasicMultipleAccessor[V, R, T, L](value: V from R)(
        implicit ev: Transmission[V, R, T, L, Multiple])
      extends RemoteAccessor {

      def asLocalFromAll: Seq[(Remote[R], T)] = value.remotes zip value.retrieveValues
    }

    implicit class BasicBlockingMultipleAccessor[V, R, T, L](value: V from R)(
        implicit ev: Transmission[V, R, Future[T], L, Multiple])
      extends RemoteAccessor {

      def asLocalFromAll_?(timeout: Duration): Seq[(Remote[R], T)] =
        value.remotes zip (Await result (Future sequence value.retrieveValues, timeout))

      def asLocalFromAll_! : Seq[(Remote[R], T)] = asLocalFromAll_?(Duration.Inf)
    }


    implicit class BasicOptionalAccessor[V, R, T, L](value: V from R)(
        implicit ev: Transmission[V, R, T, L, Optional])
      extends RemoteAccessor {

      def asLocal: Option[T] = value.retrieveValue
    }

    implicit class BasicBlockingOptionalAccessor[V, R, T, L](value: V from R)(
        implicit ev: Transmission[V, R, Future[T], L, Optional])
      extends RemoteAccessor {

      def asLocal_?(timeout: Duration): Option[T] =
        value.retrieveValue map { Await result (_, timeout) }

      def asLocal_! : Option[T] = asLocal_?(Duration.Inf)
    }


    implicit class BasicSingleAccessor[V, R, T, L](value: V from R)(
        implicit ev: Transmission[V, R, T, L, Single])
      extends RemoteAccessor {

      def asLocal: T = value.retrieveValue
    }

    implicit class BasicBlockingSingleAccessor[V, R, T, L](value: V from R)(
        implicit ev: Transmission[V, R, Future[T], L, Single])
      extends RemoteAccessor {

      def asLocal_?(timeout: Duration): T =
        Await result (value.retrieveValue, timeout)

      def asLocal_! : T = asLocal_?(Duration.Inf)
    }
  }

  sealed trait Access {
    implicit class MultipleValueAccess[V, T, R, L](value: V from R)(implicit
        ev: Transmission[V, R, T, L, _]) {

      def cache[B <: AnyRef](id: Any)(body: => B): B = ev.cache(id, body)
      val remoteJoined: Notification[Remote[R]] = ev.remoteJoined
      val remoteLeft: Notification[Remote[R]] = ev.remoteLeft
      def remotes: Seq[Remote[R]] = ev.remotesReferences
      def retrieveValues: Seq[T] = ev.retrieveValues
    }
  }
}

trait RemoteAccessor extends RemoteAccessor.Access {
  implicit class OptionalValueAccess[V, T, R, L](value: V from R)(implicit
      ev: Transmission[V, R, T, L, Optional])
    extends MultipleValueAccess(value)(ev) {

    def remote: Option[Remote[R]] = ev.remotesReferences.headOption
    def retrieveValue: Option[T] = ev.retrieveValues.headOption
  }

  implicit class SingleValueAccess[V, T, R, L](value: V from R)(implicit
      ev: Transmission[V, R, T, L, Single])
    extends MultipleValueAccess(value)(ev) {

    def remote: Remote[R] = ev.remotesReferences.head
    def retrieveValue: T = ev.retrieveValues.head
  }
}
