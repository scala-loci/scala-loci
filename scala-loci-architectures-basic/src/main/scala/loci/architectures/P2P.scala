package loci
package architectures

@multitier
object P2P {
  abstract class NodePeer[N <: NodePeer[_]: PeerTypeTag] extends Peer {
    type Tie <: Multiple[N]
    implicit def connectDefault = Default.Listen[N]
  }
}
