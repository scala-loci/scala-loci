package loci.valueref

import com.github.benmanes.caffeine.cache.Caffeine
import com.github.benmanes.caffeine.cache.CaffeineSpec

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
class PeerValueCaffeineCache(spec: Option[CaffeineSpec]) extends PeerValueCache {
  private val cache = spec.map(Caffeine.from).getOrElse(Caffeine.newBuilder()).build[UUID, Any]
  override def get(key: UUID): Option[Any] = Option(cache.getIfPresent(key))
  override def put(key: UUID, value: Any): Unit = cache.put(key, value)
}
