package loci
package architectures

@multitier
object ClientServer {
  abstract class ServerPeer[C <: ClientPeer[_]: PeerTypeTag] extends Peer {
    type Tie <: Single[C]
    implicit def connectDefault = Default.Listen[C]
  }

  abstract class ClientPeer[S <: ServerPeer[_]: PeerTypeTag] extends Peer {
    type Tie <: Single[S]
    implicit def connectDefault = Default.Request[S]
  }
}
