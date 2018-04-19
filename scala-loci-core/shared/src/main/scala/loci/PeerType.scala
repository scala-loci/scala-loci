package loci

import scala.reflect.ClassTag
import scala.annotation.implicitNotFound
import scala.language.experimental.macros

sealed trait PeerType extends Equals with PartiallyOrdered[PeerType] {
  def name: String
  def bases: List[PeerType]

  def tryCompareTo[P >: PeerType](that: P)
    (implicit ev: P => PartiallyOrdered[P]): Option[Int] = that match {
      case that: PeerType if that canEqual this =>
        def contains(bases: List[PeerType], peerType: PeerType): Boolean =
          bases exists { base =>
            base.name == peerType.name || contains(base.bases, peerType)
          }

        if (name == that.name) Some(0)
        else if (contains(this.bases, that)) Some(-1)
        else if (contains(that.bases, this)) Some(1)
        else None

      case _ =>
        None
    }
}

private trait PeerTypeImplBase extends PeerType


@implicitNotFound("No peer type information available for ${P} (maybe peer definition was not placed inside `multitier` environment)")
sealed trait PeerTypeTag[P] {
  def peerType: PeerType
}

private trait PeerTypeTagImplBase[P] extends PeerTypeTag[P]

object PeerTypeTag {
  implicit def materializePeerTypeTag[P <: Peer: ClassTag]: PeerTypeTag[P] =
    macro impl.PeerTypeTag.impl[P]
}
