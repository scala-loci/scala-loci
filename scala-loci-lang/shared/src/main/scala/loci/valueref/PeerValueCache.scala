package loci.valueref

import java.util.UUID
import scala.collection.mutable

trait PeerValueCache {
  def get(key: UUID): Option[Any]
  def getAs[V](key: UUID): Option[V] = get(key).map(_.asInstanceOf[V])
  def apply(key: UUID): Any = get(key).get
  def put(key: UUID, value: Any): Unit
}

class PeerValueMapCache extends PeerValueCache {
  private val map = mutable.Map.empty[UUID, Any]
  override def get(key: UUID): Option[Any] = map.get(key)
  override def put(key: UUID, value: Any): Unit = map.put(key, value)
}
