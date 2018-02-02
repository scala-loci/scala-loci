package loci

import scala.annotation.implicitNotFound

sealed abstract class CurrentLocalPeer[+P <: Peer]

sealed abstract class CurrentLocalPeerRemoteComputation[+P <: Peer]
  extends CurrentLocalPeer[P]

@implicitNotFound("Expression must be placed on a peer.")
final abstract class LocalPeer[+P <: Peer]

object LocalPeer {
  implicit def localPeer[P <: Peer]
    (implicit
        ev0: CurrentLocalPeer[P],
        ev1: CurrentLocalPeer[_]): LocalPeer[P] = `#macro`(ev0, ev1)
}

@implicitNotFound("Expression must not be placed on a peer.")
final abstract class NoLocalPeer[P <: Peer]

object NoLocalPeer {
  implicit def noLocalPeer[P <: Peer]: NoLocalPeer[P] = `#macro`
  implicit def noLocalPeerAmbiguousEvidence[P <: Peer]
    (implicit ev: LocalPeer[P]): NoLocalPeer[P] = `#macro`(ev)
}
