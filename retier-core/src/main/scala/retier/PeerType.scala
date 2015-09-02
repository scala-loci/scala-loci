package retier

import scala.reflect.ClassTag
import scala.annotation.implicitNotFound
import scala.language.experimental.macros

sealed trait PeerType {
  def name: String
  def bases: List[PeerType]
}

private trait PeerTypeImplBase extends PeerType


@implicitNotFound(
  "No peer type information available for ${P} " +
  "(maybe peer definition was not placed inside `multitier` environment)")
sealed trait PeerTypeTag[P] {
  def peerType: PeerType
}

private trait PeerTypeTagImplBase[P] extends PeerTypeTag[P]

object PeerTypeTag {
  implicit def materializePeerTypeTag[P <: Peer: ClassTag]: PeerTypeTag[P] =
    macro impl.PeerTypeTag.impl[P]
}
