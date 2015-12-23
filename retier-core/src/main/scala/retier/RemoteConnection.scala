package retier

import network.ConnectionRequestor
import util.Notification

sealed trait RemoteConnection[R <: Peer, M <: ConnectionMultiplicity] {
  val id: Any
  def memo[T <: AnyRef](id: Any)(body: => T): T

  val remoteJoined: Notification[Remote[R]]
  val remoteLeft: Notification[Remote[R]]
  def remotes: Seq[Remote[R]]
  def request(requestor: ConnectionRequestor): Unit
}

sealed trait MultipleRemoteConnection[R <: Peer]
    extends RemoteConnection[R, MultipleConnection] {
  def remotes: Seq[Remote[R]]
}

sealed trait OptionalRemoteConnection[R <: Peer]
    extends RemoteConnection[R, OptionalConnection] {
  def remote: Option[Remote[R]]
  def remotes: Seq[Remote[R]] = remote.toSeq
}

sealed trait SingleRemoteConnection[R <: Peer]
    extends RemoteConnection[R, SingleConnection] {
  def remote: Remote[R]
  def remotes: Seq[Remote[R]] = Seq(remote)
}


private[retier] trait MultipleRemoteConnectionImplBase[R <: Peer]
  extends MultipleRemoteConnection[R]

private[retier] trait OptionalRemoteConnectionImplBase[R <: Peer]
  extends OptionalRemoteConnection[R]

private[retier] trait SingleRemoteConnectionImplBase[R <: Peer]
  extends SingleRemoteConnection[R]


object MultipleRemoteConnection {
  implicit class DefaultMultipleRemoteConnection[R <: Peer](
    connection: MultipleRemoteConnection[R])
      extends RemoteConnectionInterface {
    val joined = connection.remoteJoined
    val left = connection.remoteLeft
    def connected = connection.remotes
    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }
}

object OptionalRemoteConnection {
  implicit class DefaultOptionalRemoteConnection[R <: Peer](
    connection: OptionalRemoteConnection[R])
      extends RemoteConnectionInterface {
    val joined = connection.remoteJoined
    val left = connection.remoteLeft
    def connected = connection.remote
    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }
}

object SingleRemoteConnection {
  implicit class DefaultSingleRemoteConnection[R <: Peer](
    connection: SingleRemoteConnection[R])
      extends RemoteConnectionInterface {
    val joined = connection.remoteJoined
    val left = connection.remoteLeft
    def connected = connection.remote
    def connect(requestor: ConnectionRequestor) = connection.request(requestor)
  }
}
