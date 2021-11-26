package loci.valueref

object PeerValueCacheProvider {
  def create(): PeerValueCache = new PeerValueMapCache
}
