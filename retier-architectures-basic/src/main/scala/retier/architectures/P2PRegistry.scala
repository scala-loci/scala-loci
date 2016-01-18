package retier
package architectures

@multitier
object P2PRegistry {
  abstract class Registry[N <: Node[_, _]: PeerTypeTag] extends Peer {
    type Connection <: Multiple[N]
    implicit def connectDefault = Default.Listen[N]
  }

  abstract class Node[
      N <: Node[_, _]: PeerTypeTag,
      R <: Registry[_]: PeerTypeTag] extends Peer {
    type Connection <: Multiple[N] with Optional[R]
  }
}
