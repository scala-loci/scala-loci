package loci

import communicator.Connector
import messaging.ConnectionsBase.Protocol

sealed trait RemoteConnection[R <: Peer, M <: TieMultiplicity] {
  val id: Any
  def memo[T <: AnyRef](id: Any)(body: => T): T

  val remoteJoined: Notification[Remote[R]]
  val remoteLeft: Notification[Remote[R]]
  def remotes: Seq[Remote[R]]
  def connect(connector: Connector[Protocol]): Unit
}

sealed trait MultipleRemoteConnection[R <: Peer]
    extends RemoteConnection[R, MultipleTie] {
  def remotes: Seq[Remote[R]]
}

sealed trait OptionalRemoteConnection[R <: Peer]
    extends RemoteConnection[R, OptionalTie] {
  def remote: Option[Remote[R]]
  def remotes: Seq[Remote[R]] = remote.toSeq
}

sealed trait SingleRemoteConnection[R <: Peer]
    extends RemoteConnection[R, SingleTie] {
  def remote: Remote[R]
  def remotes: Seq[Remote[R]] = Seq(remote)
}


private[loci] trait MultipleRemoteConnectionImplBase[R <: Peer]
  extends MultipleRemoteConnection[R]

private[loci] trait OptionalRemoteConnectionImplBase[R <: Peer]
  extends OptionalRemoteConnection[R]

private[loci] trait SingleRemoteConnectionImplBase[R <: Peer]
  extends SingleRemoteConnection[R]


object MultipleRemoteConnection {
  implicit class DefaultMultipleRemoteConnection[R <: Peer](
    connection: MultipleRemoteConnection[R])
      extends RemoteConnectionInterface {
    val joined = connection.remoteJoined
    val left = connection.remoteLeft
    def connected = connection.remotes
    def connect(connector: Connector[Protocol]) = connection connect connector
  }
}

object OptionalRemoteConnection {
  implicit class DefaultOptionalRemoteConnection[R <: Peer](
    connection: OptionalRemoteConnection[R])
      extends RemoteConnectionInterface {
    val joined = connection.remoteJoined
    val left = connection.remoteLeft
    def connected = connection.remote
    def connect(connector: Connector[Protocol]) = connection connect connector
  }
}

object SingleRemoteConnection {
  implicit class DefaultSingleRemoteConnection[R <: Peer](
    connection: SingleRemoteConnection[R])
      extends RemoteConnectionInterface {
    val joined = connection.remoteJoined
    val left = connection.remoteLeft
    def connected = connection.remote
  }
}
