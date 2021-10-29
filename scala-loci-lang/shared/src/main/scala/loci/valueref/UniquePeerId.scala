package loci.valueref

import java.util.UUID

object UniquePeerId {
  def generate(): UUID = UUID.randomUUID()
}
