package loci
package transmitter

import communicator.ProtocolCommon
import communicator.Bidirectional
import communicator.Connector
import _root_.rescala.core.Struct
import _root_.rescala.core.Engine

protected[transmitter] trait RescalaConnectionInterface {
  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2


  protected class RescalaRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: RemoteConnection[P, _])
      (implicit val engine: Engine[S]) {
    import engine.{ Event, Evt }

    lazy val joined: Event[Remote[P]] = connection.memo(joinedId) {
      val event = Evt[Remote[P]]
      connection.remoteJoined notify event.apply
      event
    }

    lazy val left: Event[Remote[P]] = connection.memo(leftId) {
      val event = Evt[Remote[P]]
      connection.remoteLeft notify event.apply
      event
    }

    protected def update(update: => Unit) = {
      connection.remoteJoined notify { _ => update }
      connection.remoteLeft notify { _ => update }
    }
  }


  implicit class RescalaMultipleRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: MultipleRemoteConnection[P])
      (implicit override val engine: Engine[S])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {
    import engine.{ Signal, Var }

    lazy val connected: Signal[Seq[Remote[P]]] =
      connection.memo(connectedId) {
        val signal = Var(connection.remotes)
        update { signal() = connection.remotes }
        signal
      }

    def connect(connector: Connector[ProtocolCommon with Bidirectional]) =
      connection connect connector
  }

  implicit def RescalaMultipleRemoteConnectionInterfaceView
    [P <: Peer, S <: Struct]
    (implicit engine: Engine[S]) =
    { connection: MultipleRemoteConnection[P] =>
        new RescalaMultipleRemoteConnectionInterface(connection) }


  implicit class RescalaOptionalRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: OptionalRemoteConnection[P])
      (implicit override val engine: Engine[S])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {
    import engine.{ Signal, Var }

    lazy val connected: Signal[Option[Remote[P]]] =
      connection.memo(connectedId) {
        val signal = Var(connection.remote)
        update { signal() = connection.remote }
        signal
      }

    def connect(connector: Connector[ProtocolCommon with Bidirectional]) =
      connection connect connector
  }

  implicit def RescalaOptionalRemoteConnectionInterfaceView
    [P <: Peer, S <: Struct]
    (implicit engine: Engine[S]) =
    { connection: OptionalRemoteConnection[P] =>
        new RescalaOptionalRemoteConnectionInterface(connection) }


  implicit class RescalaSingleRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: SingleRemoteConnection[P])
      (implicit override val engine: Engine[S])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {

    lazy val connected: Remote[P] = connection.remote
  }

  implicit def RescalaSingleRemoteConnectionInterfaceView
    [P <: Peer, S <: Struct]
    (implicit engine: Engine[S]) =
    { connection: SingleRemoteConnection[P] =>
        new RescalaSingleRemoteConnectionInterface(connection) }
}
