package loci
package impl

import org.scalatest._

class PeerTypeSpec extends FlatSpec with Matchers with TryValues {
  behavior of "PeerType"

  private val peerTypeA = PeerTypeImpl("A", List.empty)
  private val peerTypeB0 = PeerTypeImpl("B{0}", List(peerTypeA))
  private val peerTypeB1 = PeerTypeImpl("B{1}", List(peerTypeA))
  private val peerTypeC = PeerTypeImpl("C: B0, B1", List(peerTypeB0, peerTypeB1))

  private val malformed = "Malformed peer type representation"

  it should "serialize peer types correctly" in {
    (PeerType serialize peerTypeC) should be ("C: B0\\, B1{B\\{0\\}{A},B\\{1\\}{A}}")
  }

  it should "deserialize peer types correctly" in {
    (PeerType deserialize "C: B0\\, B1{B\\{0\\}{A},B\\{1\\}{A}}").success.value should be (peerTypeC)
  }

  it should "not deserialize peer types incorrectly" in {
    (PeerType deserialize "C: B0, B1{B\\{0\\}{A},B\\{1\\}{A}}").failure.exception should have message malformed

    (PeerType deserialize "C: B0\\, B1{B\\{0\\}{A},B\\{1\\}A}}").failure.exception should have message malformed

    (PeerType deserialize "C: B0\\, B1{B\\{0\\}{A},B\\{1\\}{A}").failure.exception should have message malformed

    (PeerType deserialize "C: B0\\, B1{B{0\\}{A},B\\{1\\}{A}}").failure.exception should have message malformed

    (PeerType deserialize "C: B0\\, B1{B\\{0\\}{A}\\,B\\{1\\}{A}}").failure.exception should have message malformed

    (PeerType deserialize "C: B0\\, B1{B\\{0\\}{A}\\,B\\{1\\}{A}}").failure.exception should have message malformed
  }
}
