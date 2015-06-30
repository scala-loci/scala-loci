package retier

import typeconstraints._

final abstract class Transmission
  [V, T, R <: Peer, L <: Peer, M <: ConnectionMultiplicity]

object Transmission {
  implicit def transmission
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T `shared on` R),
        value: T <:!< (_ <=> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    Transmission[V, T, R, L, M] = `#macro`

  implicit def transmissionFromSingle
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T `from single` R),
        value: T <:!< (_ <=> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    Transmission[V, T, R, L, OptionalConnection] = `#macro`

  implicit def transmissionFromMultiple
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T `from multiple` R),
        value: T <:!< (_ <=> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    Transmission[V, T, R, L, MultipleConnection] = `#macro`

  implicit def issuedTransmission
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T `shared on` R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    Transmission[V, T, R, L, M] = `#macro`

  implicit def issuedTransmissionFromSingle
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T `from single` R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    Transmission[V, T, R, L, OptionalConnection] = `#macro`

  implicit def issuedTransmissionFromMultiple
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T `from multiple` R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    Transmission[V, T, R, L, MultipleConnection] = `#macro`
}
