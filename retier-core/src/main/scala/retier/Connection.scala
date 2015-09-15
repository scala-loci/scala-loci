package retier

import scala.annotation.implicitNotFound

sealed trait ConnectionMultiplicity
sealed trait SingleConnection extends ConnectionMultiplicity
sealed trait OptionalConnection extends ConnectionMultiplicity
sealed trait MultipleConnection extends ConnectionMultiplicity

@implicitNotFound("No connection to ${P}.")
protected final abstract class PeerConnection
  [CS <: Peer#ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]

protected object PeerConnection {
  implicit def multiple
    [CS <: Peer#ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
    (implicit
        ev0: PeerConnectionHelper[CS, P, M],
        ev1: M =:= MultipleConnection): PeerConnection[CS, P, MultipleConnection] = `#macro`
  implicit def optional
    [CS <: Peer#ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
    (implicit
        ev0: PeerConnectionHelper[CS, P, M],
        ev1: M =:= OptionalConnection): PeerConnection[CS, P, OptionalConnection] = `#macro`
  implicit def single
    [CS <: Peer#ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
    (implicit
        ev0: PeerConnectionHelper[CS, P, M],
        ev1: M =:= SingleConnection): PeerConnection[CS, P, SingleConnection] = `#macro`


  sealed trait PeerConnectionHelperSecondFallback {
    implicit def multiple
      [CS <: Peer#Multiple[P], P <: Peer]:
      PeerConnectionHelper[CS, P, MultipleConnection] = `#macro`
  }

  sealed trait PeerConnectionHelperFirstFallback
      extends PeerConnectionHelperSecondFallback {
    implicit def optional
      [CS <: Peer#Optional[P], P <: Peer]:
      PeerConnectionHelper[CS, P, OptionalConnection] = `#macro`
  }

  object PeerConnectionHelper
      extends PeerConnectionHelperFirstFallback {
    implicit def single
      [CS <: Peer#Single[P], P <: Peer]:
      PeerConnectionHelper[CS, P, SingleConnection] = `#macro`
  }

  final abstract class PeerConnectionHelper
    [CS <: Peer#ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
}

// The IntelliJ IDEA Scala Plugin cannot always resolve the implicit value
// using the following more direct formulation:
//
// protected object PeerConnection {
//   implicit def multiple
//     [CS <: Peer#ConnectionSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Peer#Multiple[P],
//       ev1: CS <:!< Peer#Optional[P],
//       ev2: CS <:!< Peer#Single[P]): PeerConnection[CS, P, MultipleConnection] = `#macro`
//   implicit def optional
//     [CS <: Peer#ConnectionSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Peer#Optional[P],
//       ev1: CS <:!< Peer#Single[P]): PeerConnection[CS, P, OptionalConnection] = `#macro`
//   implicit def single
//     [CS <: Peer#ConnectionSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Peer#Single[P]): PeerConnection[CS, P, SingleConnection] = `#macro`
// }
