package loci.valueref

import loci.Remote
import loci.language.Placed.Selected
import loci.language.PlacedValue
import loci.language.Placement.Context
import loci.valueref.ValueRefAccessors.NotConnectedToPeerWithId
import loci.valueref.ValueRefAccessors.PeerValueCacheMiss

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

trait ValueRefAccessors {

  implicit class ValueRefAccessor[V, R, P](ref: ValueRef[V, R])(
    implicit val peerId: UUID,
    implicit val cache: PeerValueCache,
    implicit val remotePeerIds: Map[UUID, Remote[R]],
    implicit val cacheValueAccess: (UUID, Remote[R]) => PlacedValue.BasicSingleAccessor[Selected.Single[Option[V]], R, Future[Option[V]], P],
    implicit val context: Context[P],
    implicit val executionContext: ExecutionContext,
  ) {
    def getValue: Future[V] = {
      ref.peerId match {
        case id if id == peerId =>
          Future.successful(cache.getAs[V](ref.valueId).getOrElse(throw PeerValueCacheMiss(ref.valueId)))
        case id => remotePeerIds.get(id) match {
          case Some(remote) => cacheValueAccess(ref.valueId, remote).asLocal.flatMap {
            case Some(value) => Future.successful(value)
            case None => Future.failed(PeerValueCacheMiss(ref.valueId))
          }
          case None => Future.failed(NotConnectedToPeerWithId(ref.peerId))
        }
      }
    }
  }
}

object ValueRefAccessors {

  case class NotConnectedToPeerWithId(peerId: UUID) extends RuntimeException(
    s"Can't access value from unconnected peer with id $peerId"
  )

  case class PeerValueCacheMiss(valueId: UUID) extends RuntimeException(
    s"Did not find value with id $valueId in cache"
  )
}
