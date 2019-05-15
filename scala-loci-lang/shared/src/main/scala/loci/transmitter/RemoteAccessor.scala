package loci
package transmitter

object RemoteAccessor {
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
