package retier

import network.ConnectionRequestor
import rescala.synchronization.Engines.default
import rescala.synchronization.Engines.default._


protected[retier] trait ReactiveRemoteConnectionInterface {
  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2

  protected class RescalaRemoteConnectionInterface[P <: Peer](
      connection: RemoteConnection[P, _]) {
    lazy val joined: Event[Remote[P]] = connection.memo(joinedId) {
      val event = Evt[Remote[P]]
      connection.remoteJoined += event.apply
      event
    }

    lazy val left: Event[Remote[P]] = connection.memo(leftId) {
      val event = Evt[Remote[P]]
      connection.remoteLeft += event.apply
      event
    }

    protected def update(update: => Unit) = {
      connection.remoteJoined += { _ => update }
      connection.remoteLeft += { _ => update }
    }
  }

  implicit class RescalaMultipleRemoteConnectionInterface[P <: Peer](
    connection: MultipleRemoteConnection[P])
      extends RescalaRemoteConnectionInterface(connection)
      with RemoteConnectionInterface {
    lazy val connected: Signal[Seq[Remote[P]]] =
      connection.memo(connectedId) {
        val signal = Var(connection.remotes)
        update { signal() = connection.remotes }
        signal
      }

    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }

  implicit class RescalaOptionalRemoteConnectionInterface[P <: Peer](
    connection: OptionalRemoteConnection[P])
      extends RescalaRemoteConnectionInterface(connection)
      with RemoteConnectionInterface {
    lazy val connected: Signal[Option[Remote[P]]] =
      connection.memo(connectedId) {
        val signal = Var(connection.remote)
        update { signal() = connection.remote }
        signal
      }

    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }

  implicit class RescalaSingleRemoteConnectionInterface[P <: Peer](
    connection: SingleRemoteConnection[P])
      extends RescalaRemoteConnectionInterface(connection)
      with RemoteConnectionInterface {
    lazy val connected: Remote[P] = connection.remote
  }
}
