package retier

import scala.reflect.ClassTag
import scala.annotation.implicitNotFound
import scala.language.experimental.macros

sealed trait PeerType {
  def name: String
}

private trait PeerTypeImplBase extends PeerType

object PeerType {
  def apply[P <: Peer: ClassTag](peer: P): PeerType =
    impl.PeerType.create(peer)
}


@implicitNotFound("No peer type information available for ${P}")
sealed trait PeerTypeTag[P] {
  def peerType: PeerType
}

private trait PeerTypeTagImplBase[P] extends PeerTypeTag[P]

object PeerTypeTag {
  implicit def materializePeerTypeTag[P <: Peer: ClassTag]: PeerTypeTag[P] =
    macro impl.PeerTypeTag.impl[P]
}
