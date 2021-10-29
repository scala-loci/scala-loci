package loci.valueref

import java.util.UUID
import scala.collection.mutable

trait PeerValueCache[V] {
  def get(key: UUID): Option[V]
  def apply(key: UUID): V
  def put(key: UUID, value: V): Unit
}

class PeerValueMapCache[V] extends PeerValueCache[V] {
  private val map = mutable.Map.empty[UUID, V]
  override def get(key: UUID): Option[V] = map.get(key)
  override def apply(key: UUID): V = map.apply(key)
  override def put(key: UUID, value: V): Unit = map.put(key, value)
}
