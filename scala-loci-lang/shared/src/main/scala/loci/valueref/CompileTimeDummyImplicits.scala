package loci.valueref

import loci.Remote
import loci.language.CanonicalPlacedTypeAlias
import loci.language.Placed.Selected
import loci.language.PlacedClean
import loci.language.PlacedValue
import loci.language.erased
import loci.language.fromSingle
import loci.on
import loci.runtime.Peer
import loci.transmitter.Single
import loci.transmitter.Transmission

import java.util.UUID
import scala.annotation.compileTimeOnly
import scala.concurrent.Future

trait CompileTimeDummyImplicits {

  @compileTimeOnly("dummyPeerId can only be invoked in multitier code and should be replaced at compile time")
  implicit def dummyPeerId: UUID = erased

  @compileTimeOnly("dummyCache can only be invoked in multitier code and should be replaced at compile time")
  implicit def dummyCache: PeerValueCache = erased

  @compileTimeOnly("dummySignature can only be invoked in multitier code and should be replaced at compile time")
  implicit def dummySignature: Peer.Signature = erased

  @compileTimeOnly("dummyRemotePeerIds can only be invoked in multitier code and should be replaced at compile time")
  implicit def dummyRemotePeerIds[R]: Map[UUID, Remote[R]] = erased

  @compileTimeOnly("dummyCacheValueAccess can only be invoked in multitier code and should be replaced at compile time")
  implicit def dummyCacheValueAccess[V, R, P](
    implicit transmission: Transmission[Selected.Single[Option[V]], R, Future[Option[V]], P, Single],
    placedClean: PlacedClean[Option[V] on R, R, Option[V], Option[V], Option[V]],
    canonicalPlacedTypeAlias: CanonicalPlacedTypeAlias[Option[V] fromSingle R, Option[V] fromSingle R]
  ): (UUID, Remote[R]) => PlacedValue.BasicSingleAccessor[Selected.Single[Option[V]], R, Future[Option[V]], P] = erased

}
