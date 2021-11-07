package loci.valueref

import loci.runtime.Peer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class ValueRefSpec extends AnyFlatSpec with Matchers {
  behavior of "ValueRef"

  type V
  type P

  private val refSerializable = ValueRef.serializable[V, P]
  private val pairSerializable = ValueRef.serializableOptionValueRefOptionString[V, P]

  private val moduleSignature = loci.runtime.Module.Signature("module", List("some", "path"))
  private val ref = ValueRef[V, P](
    UUID.randomUUID(),
    UUID.randomUUID(),
    Peer.Signature("P", List(Peer.Signature("Q", List(), moduleSignature)), moduleSignature)
  )

  private val string = "asda;asdaasd-xknnkldg; asfjjsdjjjsdfjnjsd235rgrge"

  it should "serialize and deserialize ValueRef" in {
    refSerializable.deserialize(refSerializable.serialize(ref)).get shouldEqual ref
  }

  it should "serialize and deserialize ValueRef-String option pair with both defined" in {
    val pair = (Some(ref), Some(string))
    pairSerializable.deserialize(pairSerializable.serialize(pair)).get shouldEqual pair
  }

  it should "serialize and deserialize ValueRef-String option pair with empty string" in {
    val pair = (Some(ref), Some(""))
    pairSerializable.deserialize(pairSerializable.serialize(pair)).get shouldEqual pair
  }

  it should "serialize and deserialize ValueRef-String option pair with undefined string" in {
    val pair = (Some(ref), None)
    pairSerializable.deserialize(pairSerializable.serialize(pair)).get shouldEqual pair
  }

  it should "serialize and deserialize ValueRef-String option pair with undefined ref" in {
    val pair = (None, Some(string))
    pairSerializable.deserialize(pairSerializable.serialize(pair)).get shouldEqual pair
  }

  it should "serialize and deserialize ValueRef-String option pair with both undefined" in {
    val pair = (None, None)
    pairSerializable.deserialize(pairSerializable.serialize(pair)).get shouldEqual pair
  }

}
