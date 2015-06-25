package retier

import scala.annotation.implicitNotFound

private sealed abstract class CurrentLocalPeer[+P <: Peer]

private sealed abstract class CurrentLocalPeerRemoteComputation[+P <: Peer]
  extends CurrentLocalPeer[P]

@implicitNotFound("Expression must be placed on a peer.")
private final abstract class LocalPeer[+P <: Peer]

private object LocalPeer {
  implicit def localPeer[P <: Peer]
    (implicit
        ev0: CurrentLocalPeer[P],
        ev1: CurrentLocalPeer[_]): LocalPeer[P] = `#macro`
}

@implicitNotFound("Expression must not be placed on a peer.")
private final abstract class NoLocalPeer[P <: Peer]

private object NoLocalPeer {
  implicit def noLocalPeer[P <: Peer]: NoLocalPeer[P] = `#macro`
  implicit def noLocalPeerAmbiguousEvidence[P <: Peer]
    (implicit ev: LocalPeer[P]): NoLocalPeer[P] = `#macro`
}
