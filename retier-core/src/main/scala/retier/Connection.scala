package retier

import scala.annotation.implicitNotFound

// `Single` and `Optional` are invariant in `T`, while `Multiple` is covariant.
// This is because type inference may infer a super type `S` of `T` and it is
// possible that other subtypes of S are part of the connection spec compound.
// Therefore, when inferring a super type of `T`, `Multiple` must be inferred.
sealed trait ConnectionSpec
trait Single[T] extends Optional[T]
trait Optional[T] extends Multiple[T]
trait Multiple[+T] extends ConnectionSpec

sealed class ConnectionMultiplicity
final class SingleConnection extends ConnectionMultiplicity
final class OptionalConnection extends ConnectionMultiplicity
final class MultipleConnection extends ConnectionMultiplicity

@implicitNotFound("No connection to ${P}.")
private final abstract class PeerConnection
  [CS <: ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]

private object PeerConnection {
  implicit def multiple
    [CS <: ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
    (implicit
        ev0: PeerConnectionHelper[CS, P, M],
        ev1: M =:= MultipleConnection): PeerConnection[CS, P, MultipleConnection] = `#macro`
  implicit def optional
    [CS <: ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
    (implicit
        ev0: PeerConnectionHelper[CS, P, M],
        ev1: M =:= OptionalConnection): PeerConnection[CS, P, OptionalConnection] = `#macro`
  implicit def single
    [CS <: ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
    (implicit
        ev0: PeerConnectionHelper[CS, P, M],
        ev1: M =:= SingleConnection): PeerConnection[CS, P, SingleConnection] = `#macro`


  sealed trait PeerConnectionHelperSecondFallback {
    implicit def multiple
      [CS <: Multiple[P], P <: Peer]:
      PeerConnectionHelper[CS, P, MultipleConnection] = `#macro`
  }

  sealed trait PeerConnectionHelperFirstFallback
      extends PeerConnectionHelperSecondFallback {
    implicit def optional
      [CS <: Optional[P], P <: Peer]:
      PeerConnectionHelper[CS, P, OptionalConnection] = `#macro`
  }

  object PeerConnectionHelper
      extends PeerConnectionHelperFirstFallback {
    implicit def single
      [CS <: Single[P], P <: Peer]:
      PeerConnectionHelper[CS, P, SingleConnection] = `#macro`
  }

  final abstract class PeerConnectionHelper
    [CS <: ConnectionSpec, P <: Peer, M <: ConnectionMultiplicity]
}

// The IntelliJ IDEA Scala Plugin cannot always resolve the implicit value
// using the following more direct formulation:
//
// private object PeerConnection {
//   implicit def multiple
//     [CS <: ConnectionSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Multiple[P],
//       ev1: CS <:!< Optional[P],
//       ev2: CS <:!< Single[P]): PeerConnection[CS, P, MultipleConnection] = `#macro`
//   implicit def optional
//     [CS <: ConnectionSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Optional[P],
//       ev1: CS <:!< Single[P]): PeerConnection[CS, P, OptionalConnection] = `#macro`
//   implicit def single
//     [CS <: ConnectionSpec, P <: Peer]
//     (implicit
//       ev0: CS <:< Single[P]): PeerConnection[CS, P, SingleConnection] = `#macro`
// }
