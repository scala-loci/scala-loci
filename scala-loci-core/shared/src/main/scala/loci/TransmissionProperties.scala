package loci

import typeconstraints._

final abstract class TransmissionProperties
  [V, T, R <: Peer, L <: Peer, M <: ConnectionMultiplicity]

object TransmissionProperties {
  implicit def transmission
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T sharedOn R),
        nonControlledIssued: T <:!< (_ <=> _),
        nonIssued: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    TransmissionProperties[V, T, R, L, M] = `#macro`

  implicit def transmissionFromSelected
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T from R),
        nonControlledIssued: T <:!< (_ <=> _),
        nonIssued: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    TransmissionProperties[V, T, R, L, M] = `#macro`

  implicit def transmissionFromSelectedSingle
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T fromSingle R),
        nonControlledIssued: T <:!< (_ <=> _),
        nonIssued: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    TransmissionProperties[V, T, R, L, SingleConnection] = `#macro`

  implicit def transmissionFromSelectedMultiple
    [V, L <: Peer, R <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (T fromMultiple R),
        nonControlledIssued: T <:!< (_ <=> _),
        nonIssued: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M]):
    TransmissionProperties[V, T, R, L, MultipleConnection] = `#macro`

  implicit def controlledIssuedTransmission
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T sharedOn R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] = `#macro`

  implicit def controlledIssuedTransmissionFromSelected
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T from R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] = `#macro`

  implicit def controlledIssuedTransmissionFromSelectedSingle
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T fromSingle R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, SingleConnection] = `#macro`

  implicit def controlledIssuedTransmissionFromSelectedMultiple
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T fromMultiple R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, MultipleConnection] = `#macro`

  implicit def issuedTransmission
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T sharedOn R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] = `#macro`

  implicit def issuedTransmissionFromSelected
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T from R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] = `#macro`

  implicit def issuedTransmissionFromSelectedSingle
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T fromSingle R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, SingleConnection] = `#macro`

  implicit def issuedTransmissionFromSelectedMultiple
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: ConnectionMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T fromMultiple R),
        localPeer: LocalPeer[L],
        connection: PeerConnection[L#Connection, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, MultipleConnection] = `#macro`
}
