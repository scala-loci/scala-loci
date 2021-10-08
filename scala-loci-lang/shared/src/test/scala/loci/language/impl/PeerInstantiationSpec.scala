package loci
package language
package impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PeerInstantiationSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Peer instantiation"

  it should "compile when instantiating a concrete peer" in {
    """@multitier object Module {
      @peergroup type Node
      @peer type A <: Node { type Tie <: Multiple[Node] }
    }

    object Node extends App {
      multitier start new loci.Instance[Module.A]()
    }""" should compile
  }

  it should "not compile when instantiating a peergroup" in {
    """@multitier object Module {
      @peergroup type Node
      @peer type A <: Node { type Tie <: Multiple[Node] }
    }

    object Node extends App {
      multitier start new loci.Instance[Module.Node]()
    }""" shouldNot compile
  }
}
