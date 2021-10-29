package loci.valueref

import java.util.UUID

case class ValueRef[V, P](peerId: UUID, valueId: UUID)
