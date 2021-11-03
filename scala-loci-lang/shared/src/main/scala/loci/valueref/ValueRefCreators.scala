package loci.valueref

import loci.language.Placement.Context

import java.util.UUID

trait ValueRefCreators {

  implicit class ValueRefCreator[V, P](value: V)(
    implicit val peerId: UUID,
    implicit val cache: PeerValueCache[V],
    implicit val context: Context[P]
  ) {
    def asValueRef: V via P = {
      val valueId = ValueIdGenerator.generate()
      cache.put(valueId, value)
      ValueRef[V, P](peerId, valueId)
    }
  }

}

private object ValueIdGenerator {
  def generate(): UUID = UUID.randomUUID()
}
