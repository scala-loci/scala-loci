package loci.dev
package runtime

import loci.transmitter.RemoteRef

final class RemoteRequest[V, T, L, M, U](
  arguments: U,
  placedValue: PlacedValue[U, _, _, _, _, T],
  peer: Peer.Signature,
  remotes: Seq[RemoteRef],
  system: System)
    extends transmitter.Transmission[V, T, L, M] {

  @inline private[dev] def requestValues: Seq[T] =
    system.invokeRemoteAccess(arguments, placedValue, peer,  remotes, requestResult = true)
}
