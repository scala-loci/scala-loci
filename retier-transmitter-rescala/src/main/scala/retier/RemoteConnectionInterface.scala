package retier

import network.ConnectionRequestor
import rescala.turns.Engine
import rescala.turns.Turn
import rescala.graph.Spores

protected[retier] trait ReactiveRemoteConnectionInterface {
  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2


  protected class RescalaRemoteConnectionInterface[P <: Peer, S <: Spores]
      (connection: RemoteConnection[P, _])
      (implicit val engine: Engine[S, Turn[S]]) {
    import engine._

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


  implicit class RescalaMultipleRemoteConnectionInterface[P <: Peer, S <: Spores]
      (connection: MultipleRemoteConnection[P])
      (implicit override val engine: Engine[S, Turn[S]])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {
    import engine._

    lazy val connected: Signal[Seq[Remote[P]]] =
      connection.memo(connectedId) {
        val signal = Var(connection.remotes)
        update { signal() = connection.remotes }
        signal
      }

    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }

  implicit def RescalaMultipleRemoteConnectionInterfaceView
    [P <: Peer, S <: Spores]
    (implicit engine: Engine[S, Turn[S]]) =
    { connection: MultipleRemoteConnection[P] =>
        new RescalaMultipleRemoteConnectionInterface(connection) }


  implicit class RescalaOptionalRemoteConnectionInterface[P <: Peer, S <: Spores]
      (connection: OptionalRemoteConnection[P])
      (implicit override val engine: Engine[S, Turn[S]])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {
    import engine._

    lazy val connected: Signal[Option[Remote[P]]] =
      connection.memo(connectedId) {
        val signal = Var(connection.remote)
        update { signal() = connection.remote }
        signal
      }

    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }

  implicit def RescalaOptionalRemoteConnectionInterfaceView
    [P <: Peer, S <: Spores]
    (implicit engine: Engine[S, Turn[S]]) =
    { connection: OptionalRemoteConnection[P] =>
        new RescalaOptionalRemoteConnectionInterface(connection) }


  implicit class RescalaSingleRemoteConnectionInterface[P <: Peer, S <: Spores]
      (connection: SingleRemoteConnection[P])
      (implicit override val engine: Engine[S, Turn[S]])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {

    lazy val connected: Remote[P] = connection.remote
  }

  implicit def RescalaSingleRemoteConnectionInterfaceView
    [P <: Peer, S <: Spores]
    (implicit engine: Engine[S, Turn[S]]) =
    { connection: SingleRemoteConnection[P] =>
        new RescalaSingleRemoteConnectionInterface(connection) }
}
