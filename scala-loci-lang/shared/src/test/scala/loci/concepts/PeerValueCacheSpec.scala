package loci
package concepts

import loci.valueref._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@multitier object PeerValueCacheModule {
  @peer type Node

  def f(x: Int): Local[Int at Node] on Node = on[Node] local { implicit! =>
    remote ref x
  }
}

class PeerValueCacheSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Peer value cache"

  it should "add referenced values to the cache" in {
    val node = multitier start new Instance[PeerValueCacheModule.Node](contexts.Immediate.global)

    for (x <- 1 to 10) {
      node.instance.current.foreach { inst =>
        val ref = inst retrieve PeerValueCacheModule.f(x)
        ref.peerId shouldEqual inst.retrieveUniquePeerId()
        val cache = inst.retrievePeerValueCache()
        cache(ref.valueId) shouldEqual x
      }
    }
  }
}
