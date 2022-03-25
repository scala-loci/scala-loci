package loci.valueref

import com.github.benmanes.caffeine.cache.CaffeineSpec

object PeerValueCacheProvider {
  def create(caffeineSpec: Option[CaffeineSpec]): PeerValueCache = new PeerValueMapCache
}
