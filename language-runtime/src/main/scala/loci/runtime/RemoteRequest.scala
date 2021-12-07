package loci
package runtime

import transmitter.RemoteRef

final class RemoteRequest[V, R, T, L, M, U](
  arguments: U,
  placedValue: PlacedValue[U, _, _, T],
  peer: Peer.Signature,
  remotes: Seq[RemoteRef],
  instances: Boolean,
  system: System)
    extends language.transmitter.Transmission[V, R, T, L, M] {

  private[this] val remoteId: AnyRef = (placedValue, peer, remotes, instances, system)

  @inline private[loci] def cache[B <: AnyRef](id: Any, body: => B): B =
    if (placedValue.stable)
      system.cache((remoteId, id), body)
    else
      body

  @inline private[loci] val remoteJoined: Notice.Stream[Remote[R]] = {
    if (instances && remotes.isEmpty)
      Notice.Stream[Remote[R]].notice
    else
      system.remoteJoined(peer, remotes, earlyAccess = true)
  }

  @inline private[loci] val remoteLeft: Notice.Stream[Remote[R]] =
    if (instances && remotes.isEmpty)
      Notice.Stream[Remote[R]].notice
    else
      system.remoteLeft(peer, remotes, earlyAccess = true)

  @inline private[loci] def remotesReferences: Seq[Remote[R]] = {
    if (instances && remotes.isEmpty)
      Seq.empty
    else
      system.remoteReferences(peer, remotes, earlyAccess = true)
  }

  @inline private[loci] def retrieveValues: Seq[T] =
    if (instances && remotes.isEmpty)
      Seq.empty
    else
      system.invokeRemoteAccess(arguments, placedValue, peer, remotes, requestResult = true)
}
