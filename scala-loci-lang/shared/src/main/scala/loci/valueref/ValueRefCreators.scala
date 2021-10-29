package loci.valueref

import loci.language.Placement.Context
import loci.language.erased

import java.util.UUID
import scala.annotation.compileTimeOnly

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

  @compileTimeOnly("compileTimePeerId can only be invoked in multitier code and should be replaced at compile time")
  implicit def compileTimePeerId: UUID = erased

  @compileTimeOnly("compileTimeCache can only be invoked in multitier code and should be replaced at compile time")
  implicit def compileTimeCache[V]: PeerValueCache[V] = erased

}

private object ValueIdGenerator {
  def generate(): UUID = UUID.randomUUID()
}
