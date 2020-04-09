package loci
package runtime

import transmitter.RemoteRef

final class RemoteRequest[V, R, T, L, M, U](
  arguments: U,
  placedValue: PlacedValue[U, T],
  peer: Peer.Signature,
  remotes: Seq[RemoteRef],
  system: System)
    extends transmitter.Transmission[V, R, T, L, M] {

  private[this] val remoteId = (placedValue, peer, remotes, system)

  @inline private[loci] def cache[B <: AnyRef](id: Any, body: => B): B =
    if (placedValue.stable)
      system.cache((remoteId, id), body)
    else
      body

  @inline private[loci] val remoteJoined: Notice.Stream[Remote[R]] =
    system.remoteJoined(peer, remotes, earlyAccess = true)

  @inline private[loci] val remoteLeft: Notice.Stream[Remote[R]] =
    system.remoteLeft(peer, remotes, earlyAccess = true)

  @inline private[loci] def remotesReferences: Seq[Remote[R]] =
    system.remoteReferences(peer, remotes, earlyAccess = true)

  @inline private[loci] def retrieveValues: Seq[T] =
    system.invokeRemoteAccess(arguments, placedValue, peer, remotes, requestResult = true)
}
