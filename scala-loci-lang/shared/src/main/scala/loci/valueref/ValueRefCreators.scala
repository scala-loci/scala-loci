package loci.valueref

import loci.language.Placement.Context
import loci.runtime.Peer

import java.util.UUID

trait ValueRefCreators {

  implicit class ValueRefCreator[V, P](value: V)(
    implicit val peerId: UUID,
    implicit val cache: PeerValueCache,
    implicit val signature: Peer.Signature,
    implicit val context: Context[P]
  ) {
    def asValueRef: V via P = {
      val valueId = ValueIdGenerator.generate()
      cache.put(valueId, value)
      ValueRef[V, P](peerId, valueId, signature)
    }
  }

}

private object ValueIdGenerator {
  def generate(): UUID = UUID.randomUUID()
}
