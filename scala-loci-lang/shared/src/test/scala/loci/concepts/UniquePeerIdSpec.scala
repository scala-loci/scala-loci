package loci
package concepts
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@multitier object UniquePeerIdModule {
  @peer type Node
}

class UniquePeerIdSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Unique peer id"

  it should "generate different ids on different instances" in {
    val nodeA = multitier start new Instance[UniquePeerIdModule.Node](contexts.Immediate.global)
    val nodeB = multitier start new Instance[UniquePeerIdModule.Node](contexts.Immediate.global)

    val idA = nodeA.instance.current.map(_.retrieveUniquePeerId())
    val idB = nodeB.instance.current.map(_.retrieveUniquePeerId())

    idA should be(defined)
    idB should be(defined)
    idA.get should not equal idB.get
  }

}
