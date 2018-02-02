package loci

import scala.annotation.implicitNotFound

sealed trait TieMultiplicity
sealed trait SingleTie extends TieMultiplicity
sealed trait OptionalTie extends TieMultiplicity
sealed trait MultipleTie extends TieMultiplicity

@implicitNotFound("No tie to peer.")
protected final abstract class PeerTie
  [CS <: Peer#TieSpec, P <: Peer, M <: TieMultiplicity]

protected object PeerTie {
  implicit def multiple
    [CS <: Peer#TieSpec, P <: Peer, M <: TieMultiplicity]
    (implicit
        ev0: PeerTieHelper[CS, P, M],
        ev1: M =:= MultipleTie): PeerTie[CS, P, MultipleTie] = `#macro`(ev0, ev1)
  implicit def optional
    [CS <: Peer#TieSpec, P <: Peer, M <: TieMultiplicity]
    (implicit
        ev0: PeerTieHelper[CS, P, M],
        ev1: M =:= OptionalTie): PeerTie[CS, P, OptionalTie] = `#macro`(ev0, ev1)
  implicit def single
    [CS <: Peer#TieSpec, P <: Peer, M <: TieMultiplicity]
    (implicit
        ev0: PeerTieHelper[CS, P, M],
        ev1: M =:= SingleTie): PeerTie[CS, P, SingleTie] = `#macro`(ev0, ev1)


  sealed trait PeerTieHelperSecondFallback {
    implicit def multiple
      [CS <: Peer#Multiple[P], P <: Peer]:
      PeerTieHelper[CS, P, MultipleTie] = `#macro`
  }

  sealed trait PeerTieHelperFirstFallback
      extends PeerTieHelperSecondFallback {
    implicit def optional
      [CS <: Peer#Optional[P], P <: Peer]:
      PeerTieHelper[CS, P, OptionalTie] = `#macro`
  }

  object PeerTieHelper
      extends PeerTieHelperFirstFallback {
    implicit def single
      [CS <: Peer#Single[P], P <: Peer]:
      PeerTieHelper[CS, P, SingleTie] = `#macro`
  }

  final abstract class PeerTieHelper
    [CS <: Peer#TieSpec, P <: Peer, M <: TieMultiplicity]
}

// The IntelliJ IDEA Scala Plugin cannot always resolve the implicit value
// using the following more direct formulation:
//
// protected object PeerTie {
//   implicit def multiple
//     [CS <: Peer#TieSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Peer#Multiple[P],
//       ev1: CS <:!< Peer#Optional[P],
//       ev2: CS <:!< Peer#Single[P]): PeerTie[CS, P, MultipleTie] = `#macro`
//   implicit def optional
//     [CS <: Peer#TieSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Peer#Optional[P],
//       ev1: CS <:!< Peer#Single[P]): PeerTie[CS, P, OptionalTie] = `#macro`
//   implicit def single
//     [CS <: Peer#TieSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Peer#Single[P]): PeerTie[CS, P, SingleTie] = `#macro`
// }
