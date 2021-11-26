package loci.valueref

import scalacache.caffeine.CaffeineCache
import scalacache.modes.sync.mode

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

/**
 * Only for use on JVM. It won't compile with Scala.js.
 */
class PeerValueCaffeineCache extends PeerValueCache {
  private val cache = CaffeineCache[Any]
  override def get(key: UUID): Option[Any] = cache.get(key).asInstanceOf[Option[Any]]
  override def put(key: UUID, value: Any): Unit = cache.put(key)(value)
}
