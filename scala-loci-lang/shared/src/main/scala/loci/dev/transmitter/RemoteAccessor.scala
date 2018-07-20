package loci.dev
package transmitter

object RemoteAccessor {
  sealed trait Access {
    implicit class MultipleValueAccess[V, T, R, L](value: V from R)(implicit
        ev: Transmission[V from R, T, L, _]) {

      def requestValues: Seq[T] = ev.requestValues
    }
  }
}

trait RemoteAccessor extends RemoteAccessor.Access {
  implicit class OptionalValueAccess[V, T, R, L](value: V from R)(implicit
      ev: Transmission[V from R, T, L, Optional])
    extends MultipleValueAccess(value)(ev) {

    def requestValue: Option[T] = ev.requestValues.headOption
  }

  implicit class SingleValueAccess[V, T, R, L](value: V from R)(implicit
      ev: Transmission[V from R, T, L, Single])
    extends MultipleValueAccess(value)(ev) {

    def requestValue: T = ev.requestValues.head
  }
}
