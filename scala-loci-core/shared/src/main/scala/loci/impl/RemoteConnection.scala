package loci
package impl

import communicator.Connector
import messaging.ConnectionsBase.Protocol

private final case class MultipleRemoteConnectionImpl[
  R <: Peer: PeerTypeTag](system: System)
    extends MultipleRemoteConnectionImplBase[R] {
  val id = (peerTypeOf[R], system)
  def memo[T <: AnyRef](id: Any)(body: => T) = system memo ((this.id, id), body)

  val remoteJoined = system.remoteJoined[R]
  val remoteLeft = system.remoteLeft[R]
  def remotes = system.remotes[R]
  def connect(connector: Connector[Protocol]) = system connect connector
}

private final case class OptionalRemoteConnectionImpl[
  R <: Peer: PeerTypeTag](system: System)
    extends OptionalRemoteConnectionImplBase[R] {
  val id = (peerTypeOf[R], system)
  def memo[T <: AnyRef](id: Any)(body: => T) = system memo ((this.id, id), body)

  val remoteJoined = system.remoteJoined[R]
  val remoteLeft = system.remoteLeft[R]
  def remote = system.optionalRemote[R]
  def connect(connector: Connector[Protocol]) = system connect connector
}

private final case class SingleRemoteConnectionImpl[
  R <: Peer: PeerTypeTag](system: System)
    extends SingleRemoteConnectionImplBase[R] {
  val id = (peerTypeOf[R], system)
  def memo[T <: AnyRef](id: Any)(body: => T) = system memo ((this.id, id), body)

  val remoteJoined = system.remoteJoined[R]
  val remoteLeft = system.remoteLeft[R]
  def remote = system.singleRemote[R]
  def connect(connector: Connector[Protocol]) = system connect connector
}
