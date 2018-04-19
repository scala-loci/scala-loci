package loci
package architectures

@multitier
object P2PRegistry {
  abstract class RegistryPeer[N <: NodePeer[_, _]: PeerTypeTag] extends Peer {
    type Connection <: Multiple[N]
    implicit def connectDefault = Default.Listen[N]
  }

  abstract class NodePeer[
      N <: NodePeer[_, _]: PeerTypeTag,
      R <: RegistryPeer[_]: PeerTypeTag] extends Peer {
    type Connection <: Multiple[N] with Optional[R]
  }
}
