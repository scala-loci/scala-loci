package retier
package architecture

@multitier
object P2P {
  abstract class Node[N <: Node[_]: PeerTypeTag] extends Peer {
    type Connection <: Multiple[N]
    implicit def connectDefault = Default.Listen[N]
  }
}
