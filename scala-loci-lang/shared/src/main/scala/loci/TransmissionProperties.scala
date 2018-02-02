package loci

import typeconstraints._

final abstract class TransmissionProperties
  [V, T, R <: Peer, L <: Peer, M <: TieMultiplicity]

object TransmissionProperties {
  implicit def transmission
    [V, L <: Peer, R <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (T sharedOn R),
        nonControlledSubjective: T <:!< (_ <=> _),
        nonSubjective: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M]):
    TransmissionProperties[V, T, R, L, M] =
      `#macro`(transmittable, nonControlledSubjective, nonSubjective, localPeer, tie)

  implicit def transmissionFromSelected
    [V, L <: Peer, R <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (T from R),
        nonControlledSubjective: T <:!< (_ <=> _),
        nonSubjective: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M]):
    TransmissionProperties[V, T, R, L, M] =
      `#macro`(transmittable, nonControlledSubjective, nonSubjective, localPeer, tie)

  implicit def transmissionFromSelectedSingle
    [V, L <: Peer, R <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (T fromSingle R),
        nonControlledSubjective: T <:!< (_ <=> _),
        nonSubjective: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M]):
    TransmissionProperties[V, T, R, L, SingleTie] =
      `#macro`(transmittable, nonControlledSubjective, nonSubjective, localPeer, tie)

  implicit def transmissionFromSelectedMultiple
    [V, L <: Peer, R <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (T fromMultiple R),
        nonControlledSubjective: T <:!< (_ <=> _),
        nonSubjective: T <:!< (_ <-> _),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M]):
    TransmissionProperties[V, T, R, L, MultipleTie] =
      `#macro`(transmittable, nonControlledSubjective, nonSubjective, localPeer, tie)

  implicit def controlledSubjectiveTransmission
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T sharedOn R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def controlledSubjectiveTransmissionFromSelected
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T from R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def controlledSubjectiveTransmissionFromSelectedSingle
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T fromSingle R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, SingleTie] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def controlledSubjectiveTransmissionFromSelectedMultiple
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <=> T fromMultiple R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, MultipleTie] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def subjectiveTransmission
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T sharedOn R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def subjectiveTransmissionFromSelected
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T from R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, M] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def subjectiveTransmissionFromSelectedSingle
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T fromSingle R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, SingleTie] =
      `#macro`(transmittable, localPeer, tie, dispatched)

  implicit def subjectiveTransmissionFromSelectedMultiple
    [V, L <: Peer, R <: Peer, P <: Peer, T, U, M <: TieMultiplicity]
    (implicit
        transmittable: V <:< (Remote[P] <-> T fromMultiple R),
        localPeer: LocalPeer[L],
        tie: PeerTie[L#Tie, R, M],
        dispatched: L <:< P):
    TransmissionProperties[V, T, R, L, MultipleTie] =
      `#macro`(transmittable, localPeer, tie, dispatched)
}
