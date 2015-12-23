package retier

import network.ConnectionRequestor
import rescala.Var
import rescala.Signal
import rescala.events.Event
import rescala.events.ImperativeEvent

protected[retier] trait ReactiveRemoteConnectionInterface {
  protected class RescalaRemoteConnectionInterface[P <: Peer](
      connection: RemoteConnection[P, _]) {
    lazy val joined: Event[Remote[P]] = {
      val event = new ImperativeEvent[Remote[P]]
      connection.remoteJoined += event.apply
      event
    }

    lazy val left: Event[Remote[P]] = {
      val event = new ImperativeEvent[Remote[P]]
      connection.remoteLeft += event.apply
      event
    }

    def connect(requestor: ConnectionRequestor) = connection.request(requestor)

    protected def update(update: => Unit) = {
      connection.remoteJoined += { _ => update }
      connection.remoteLeft += { _ => update }
    }
  }

  implicit class RescalaMultipleRemoteConnectionInterface[P <: Peer](
    connection: MultipleRemoteConnection[P])
      extends RescalaRemoteConnectionInterface(connection)
      with RemoteConnectionInterface {
    lazy val connected: Signal[Seq[Remote[P]]] = {
      val signal = Var(connection.remotes)
      update { signal() = connection.remotes }
      signal
    }
  }

  implicit class RescalaOptionalRemoteConnectionInterface[P <: Peer](
    connection: OptionalRemoteConnection[P])
      extends RescalaRemoteConnectionInterface(connection)
      with RemoteConnectionInterface {
    lazy val connected: Signal[Option[Remote[P]]] = {
      val signal = Var(connection.remote)
      update { signal() = connection.remote }
      signal
    }
  }

  implicit class RescalaSingleRemoteConnectionInterface[P <: Peer](
    connection: SingleRemoteConnection[P])
      extends RescalaRemoteConnectionInterface(connection)
      with RemoteConnectionInterface {
    lazy val connected: Remote[P] = connection.remote
  }
}
