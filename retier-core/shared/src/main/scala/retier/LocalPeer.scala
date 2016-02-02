package retier

import scala.annotation.implicitNotFound

protected sealed abstract class CurrentLocalPeer[+P <: Peer]

protected sealed abstract class CurrentLocalPeerRemoteComputation[+P <: Peer]
  extends CurrentLocalPeer[P]

@implicitNotFound("Expression must be placed on a peer.")
protected final abstract class LocalPeer[+P <: Peer]

protected object LocalPeer {
  implicit def localPeer[P <: Peer]
    (implicit
        ev0: CurrentLocalPeer[P],
        ev1: CurrentLocalPeer[_]): LocalPeer[P] = `#macro`
}

@implicitNotFound("Expression must not be placed on a peer.")
protected final abstract class NoLocalPeer[P <: Peer]

protected object NoLocalPeer {
  implicit def noLocalPeer[P <: Peer]: NoLocalPeer[P] = `#macro`
  implicit def noLocalPeerAmbiguousEvidence[P <: Peer]
    (implicit ev: LocalPeer[P]): NoLocalPeer[P] = `#macro`
}
