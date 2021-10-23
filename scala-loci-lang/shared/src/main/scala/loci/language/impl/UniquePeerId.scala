package loci.language.impl

import java.util.UUID

object UniquePeerId {
  def generate(): UUID = UUID.randomUUID()
}
