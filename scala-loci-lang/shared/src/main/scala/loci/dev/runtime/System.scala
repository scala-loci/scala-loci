package loci.dev
package runtime

import loci.transmitter.RemoteRef

class System {
  def setup(peer: PlacedValues): Unit = { }

  def invokeRemoteAccess[U, T](
      arguments: U,
      placedValue: PlacedValue[U, _, _, _, _, T],
      peer: Peer.Signature,
      remotes: Seq[RemoteRef],
      requestResult: Boolean): Seq[T] = ???
}
