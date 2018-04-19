package loci

import typeconstraints._
import scala.language.implicitConversions

protected object ImplicitConversions {
  trait SharedOn {
    implicit class FromExpression[L <: Peer, R <: Peer, T]
        (v: T sharedOn R)
        (implicit
          ev0: LocalPeer[L],
          ev1: PeerConnection[L#Connection, R, _]) {
      def from[P <: R]: T from P = `#macro`
      def from[P <: R](peer: Remote[P]): T fromSingle P = `#macro`
      def from[P <: R](peers: Remote[P]*): T fromMultiple P = `#macro`
    }
  }

  trait LocalOn {
    implicit class ToExpression[L <: Peer, P <: Peer, R <: Remote[Peer], T, U]
        (v: T sharedOn L)
        (implicit
          ev0: LocalPeer[L],
          ev1: RemoteValueTypes[T sharedOn L, R, U],
          ev2: R <:< Remote[P],
          ev3: PeerConnection[L#Connection, P, _]) {
      def to(peer: R): U = `#macro`
    }


    implicit def discardValue[P <: Peer](v: _ localOn P): Unit on P = `#macro`

    implicit def issueValue[P <: Peer, R <: Remote[Peer], T](v: T localOn P)
      (implicit ev0: T <:!< (_ <=> _), ev1: T <:!< (_ <-> _)): R <-> T on P = `#macro`

    implicit def issueValueControlled[P <: Peer, R <: Remote[Peer], T, U](v: T localOn P)
      (implicit ev: T <:< (R => U)): R <=> U on P = `#macro`


    implicit def downcastValueGlobally[P <: Peer, T](v: T localOn P)
      (implicit ev: NoLocalPeer[_]): T on P = `#macro`

    implicit def downcastValueLocally[P <: Peer, T](v: T localOn P)
      (implicit ev: LocalPeer[P]): T on P = `#macro`
  }

  trait AnyFallback {
    implicit def lociPlacedValue[T, U](v: T)
      (implicit ev0: LocalValueTypes[T, U], ev1: T =:!= U): U = `#macro`
  }

  trait Any extends AnyFallback {
    implicit def lociLocalPlacedValue[T <: (_ localOn _), U](v: T)
      (implicit ev: LocalValueTypes[T, U]): U = `#macro`


    implicit def lociLiftLocalPlacedValueGlobally[P <: Peer, T](v: T)
      (implicit ev0: NoLocalPeer[_], ev1: T <:!< (_ localOn _)): T on P = `#macro`

    implicit def lociLiftLocalPlacedValueLocally[P <: Peer, T](v: T)
      (implicit ev0: LocalPeer[P], ev1: T <:!< (_ localOn _)): T on P = `#macro`
  }
}
