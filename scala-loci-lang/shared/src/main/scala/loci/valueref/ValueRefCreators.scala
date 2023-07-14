package loci.valueref

import loci.language.Placement.Context
import loci.runtime.Peer

import java.util.UUID

trait ValueRefCreators {

  object remote {
    def ref[V, P](value: V)(implicit
      peerId: UUID,
      cache: PeerValueCache,
      signature: Peer.Signature,
      context: Context[P]
    ): V at P = {
      val valueId = ValueIdGenerator.generate()
      cache.put(valueId, value)
      ValueRef[V, P](peerId, valueId, signature)
    }
  }

}

private object ValueIdGenerator {
  def generate(): UUID = UUID.randomUUID()
}
