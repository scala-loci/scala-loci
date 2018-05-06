package loci
package transmitter

import communicator.ProtocolCommon
import communicator.Bidirectional
import communicator.Connector
import _root_.rescala.core.Struct
import _root_.rescala.core.Scheduler
import _root_.rescala.macros.cutOutInReactiveMacro

protected[transmitter] trait RescalaConnectionInterface {
  private final val joinedId = 0
  private final val leftId = 1
  private final val connectedId = 2


  protected class RescalaRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: RemoteConnection[P, _])
      (implicit val scheduler: Scheduler[S]) {
    import scheduler.{ Event, Evt }

    lazy val joined: Event[Remote[P]] @cutOutInReactiveMacro = connection.memo(joinedId) {
      val event = Evt[Remote[P]]
      connection.remoteJoined notify event.fire
      event
    }

    lazy val left: Event[Remote[P]] @cutOutInReactiveMacro = connection.memo(leftId) {
      val event = Evt[Remote[P]]
      connection.remoteLeft notify event.fire
      event
    }

    protected def update(update: => Unit) = {
      connection.remoteJoined notify { _ => update }
      connection.remoteLeft notify { _ => update }
    }
  }


  implicit class RescalaMultipleRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: MultipleRemoteConnection[P])
      (implicit override val scheduler: Scheduler[S])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {
    import scheduler.{ Signal, Var }

    lazy val connected: Signal[Seq[Remote[P]]] @cutOutInReactiveMacro =
      connection.memo(connectedId) {
        val signal = Var(connection.remotes)
        update { signal set connection.remotes }
        signal
      }

    def connect(connector: Connector[ProtocolCommon with Bidirectional]) =
      connection connect connector
  }

  implicit def RescalaMultipleRemoteConnectionInterfaceView
    [P <: Peer, S <: Struct]
    (implicit scheduler: Scheduler[S]) =
    { connection: MultipleRemoteConnection[P] =>
        new RescalaMultipleRemoteConnectionInterface(connection) }


  implicit class RescalaOptionalRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: OptionalRemoteConnection[P])
      (implicit override val scheduler: Scheduler[S])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {
    import scheduler.{ Signal, Var }

    lazy val connected: Signal[Option[Remote[P]]] @cutOutInReactiveMacro =
      connection.memo(connectedId) {
        val signal = Var(connection.remote)
        update { signal set connection.remote }
        signal
      }

    def connect(connector: Connector[ProtocolCommon with Bidirectional]) =
      connection connect connector
  }

  implicit def RescalaOptionalRemoteConnectionInterfaceView
    [P <: Peer, S <: Struct]
    (implicit scheduler: Scheduler[S]) =
    { connection: OptionalRemoteConnection[P] =>
        new RescalaOptionalRemoteConnectionInterface(connection) }


  implicit class RescalaSingleRemoteConnectionInterface[P <: Peer, S <: Struct]
      (connection: SingleRemoteConnection[P])
      (implicit override val scheduler: Scheduler[S])
    extends RescalaRemoteConnectionInterface[P, S](connection)
    with RemoteConnectionInterface {

    lazy val connected: Remote[P] = connection.remote
  }

  implicit def RescalaSingleRemoteConnectionInterfaceView
    [P <: Peer, S <: Struct]
    (implicit scheduler: Scheduler[S]) =
    { connection: SingleRemoteConnection[P] =>
        new RescalaSingleRemoteConnectionInterface(connection) }
}
