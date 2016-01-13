package retier
package architecture

@multitier
object MultiClientServer {
  abstract class ServerPeer[C <: ClientPeer[_]: PeerTypeTag] extends Peer {
    type Connection <: Multiple[C]
    implicit def connectDefault = Default.Listen[C]
  }

  abstract class ClientPeer[S <: ServerPeer[_]: PeerTypeTag] extends Peer {
    type Connection <: Single[S]
    implicit def connectDefault = Default.Request[S]
  }
}
