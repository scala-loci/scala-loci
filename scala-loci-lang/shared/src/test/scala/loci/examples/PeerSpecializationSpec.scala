package loci
package examples

import transmitter.Serializables._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


@multitier trait PeerSpecialization {
  @peer type A
  @peer type A0 <: A
  @peer type A1 <: A

  val value =
    (on[A] local { "value A" }) and
    (on[A0] local { "value A0" })

  def method() =
    (on[A] local { "method A" }) and
    (on[A0] local { "method A0" })

  def subjective() =
    (on[A] sbj { remote: Remote[A] => s"remote `$remote` from A" }) and
    (on[A0] sbj { remote: Remote[A] => s"remote `$remote` from A0" })

  val result = on[A] local { value -> method() }
}

@multitier object peerSpecializationApp extends PeerSpecialization {
  override def method() =
    (on[A] local { super.method() }) and
    (on[A0] local { "overridden method A0" }) and
    (on[A1] local { "overridden method A1" })
}


class PeerSpecializationSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Peer specialization example"

  it should "run correctly" in {
    val instanceA = multitier start new Instance[peerSpecializationApp.A](
      contexts.Immediate.global)
    val instanceA0 = multitier start new Instance[peerSpecializationApp.A0](
      contexts.Immediate.global)
    val instanceA1 = multitier start new Instance[peerSpecializationApp.A1](
      contexts.Immediate.global)

    val resultA = instanceA.instance map { _ retrieve peerSpecializationApp.result }
    val resultA0 = instanceA0.instance map { _ retrieve peerSpecializationApp.result }
    val resultA1 = instanceA1.instance map { _ retrieve peerSpecializationApp.result }

    val subjectiveA = instanceA.instance map { _ retrieve peerSpecializationApp.subjective() to null }
    val subjectiveA0 = instanceA0.instance map { _ retrieve peerSpecializationApp.subjective() to null }
    val subjectiveA1 = instanceA1.instance map { _ retrieve peerSpecializationApp.subjective() to null }

    resultA.current should be (Some("value A" -> "method A"))
    resultA0.current should be (Some("value A0" -> "overridden method A0"))
    resultA1.current should be (Some("value A" -> "overridden method A1"))

    subjectiveA.current should be (Some("remote `null` from A"))
    subjectiveA0.current should be (Some("remote `null` from A0"))
    subjectiveA1.current should be (Some("remote `null` from A"))

    instanceA1.terminate()
    instanceA0.terminate()
    instanceA.terminate()
  }
}
