package loci
package language
package impl

import transmitter.Serializables._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MacroExpansionJVMSpec extends AnyFlatSpec with Matchers {
  behavior of "Macro Expansion"

  def emptySystem(peer: runtime.PlacedValues) = new runtime.System(
    peer,
    runtime.Peer.Signature("", List.empty, runtime.Module.Signature("", List.empty)),
    None,
    false,
    Map.empty,
    contexts.Immediate.global,
    new runtime.RemoteConnections(
      runtime.Peer.Signature("", List.empty, runtime.Module.Signature("", List.empty)),
      Map.empty),
    Seq.empty,
    Seq.empty)

  it should "correctly compile remote blocks" in {
    @multitier object mod {
      @peer type Peer <: { type Tie <: Single[Peer] }

      val dummyRemote: Remote[Peer] = null

      def method() = on[Peer] { implicit! =>
        val i = 100

        (on[Peer].run { implicit! => identity(42) }).asLocal
        (on[Peer].run sbj { implicit! => remote: Remote[Peer] => identity(42) }).asLocal
        (on[Peer].run.capture(i) { implicit! => identity(i) }).asLocal
        (on[Peer].run.capture(i) sbj { implicit! => remote: Remote[Peer] => identity(i) }).asLocal
        (on(dummyRemote).run { implicit! => identity(42) }).asLocal
        (on(dummyRemote).run sbj { implicit! => remote: Remote[Peer] => identity(42) }).asLocal
        (on(dummyRemote).run.capture(i) { implicit! => identity(i) }).asLocal
        (on(dummyRemote).run.capture(i) sbj { implicit! => remote: Remote[Peer] => identity(i) }).asLocal
        (on(dummyRemote, dummyRemote).run { implicit! => identity(42) }).asLocalFromAll
        (on(dummyRemote, dummyRemote).run sbj { implicit! => remote: Remote[Peer] => identity(42) }).asLocalFromAll
        (on(dummyRemote, dummyRemote).run.capture(i) { implicit! => identity(i) }).asLocalFromAll
        (on(dummyRemote, dummyRemote).run.capture(i) sbj { implicit! => remote: Remote[Peer] => identity(i) }).asLocalFromAll

        on[Peer].run { implicit! => identity(42) }
        on[Peer].run sbj { implicit! => remote: Remote[Peer] => identity(42) }
        on[Peer].run.capture(i) { implicit! => identity(i) }
        on[Peer].run.capture(i) sbj { implicit! => remote: Remote[Peer] => identity(i) }
        on(dummyRemote).run { implicit! => identity(42) }
        on(dummyRemote).run sbj { implicit! => remote: Remote[Peer] => identity(42) }
        on(dummyRemote).run.capture(i) { implicit! => identity(i) }
        on(dummyRemote).run.capture(i) sbj { implicit! => remote: Remote[Peer] => identity(i) }
        on(dummyRemote, dummyRemote).run { implicit! => identity(42) }
        on(dummyRemote, dummyRemote).run sbj { implicit! => remote: Remote[Peer] => identity(42) }
        on(dummyRemote, dummyRemote).run.capture(i) { implicit! => identity(i) }
        on(dummyRemote, dummyRemote).run.capture(i) sbj { implicit! => remote: Remote[Peer] => identity(i) }
      }
    }

    val dummyRemote: Remote[mod.Peer] = null

    val peer = new mod.$loci$peer$loci$language$impl$MacroExpansionJVMSpec$_$mod$Peer {
      def $loci$sys$create = emptySystem(this)
    }

    val methods = peer.getClass.getMethods

    def invoke[T](name: String, arguments: AnyRef*) = (methods collectFirst {
      case method if method.getName == name =>
        method
    }).get.invoke(peer, arguments: _*).asInstanceOf[T]

    invoke[AnyRef]("$loci$anonymous$0") should be (42)
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$1")(dummyRemote) should be (42)
    invoke[AnyRef]("$loci$anonymous$2", Int.box(200)) should be (200)
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$3", Int.box(200))(dummyRemote) should be (200)
    invoke[AnyRef]("$loci$anonymous$4") should be (42)
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$5")(dummyRemote) should be (42)
    invoke[AnyRef]("$loci$anonymous$6", Int.box(200)) should be (200)
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$7", Int.box(200))(dummyRemote) should be (200)
    invoke[AnyRef]("$loci$anonymous$8") should be (42)
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$9")(dummyRemote) should be (42)
    invoke[AnyRef]("$loci$anonymous$10", Int.box(200)) should be (200)
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$11", Int.box(200))(dummyRemote) should be (200)

    invoke[AnyRef]("$loci$anonymous$12") should (be (()) or be (null))
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$13")(dummyRemote) should (be (()) or be (null))
    invoke[AnyRef]("$loci$anonymous$14", Int.box(200)) should (be (()) or be (null))
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$15", Int.box(200))(dummyRemote) should (be (()) or be (null))
    invoke[AnyRef]("$loci$anonymous$16") should (be (()) or be (null))
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$17")(dummyRemote) should (be (()) or be (null))
    invoke[AnyRef]("$loci$anonymous$18", Int.box(200)) should (be (()) or be (null))
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$19", Int.box(200))(dummyRemote) should (be (()) or be (null))
    invoke[AnyRef]("$loci$anonymous$20") should (be (()) or be (null))
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$21")(dummyRemote) should (be (()) or be (null))
    invoke[AnyRef]("$loci$anonymous$22", Int.box(200)) should (be (()) or be (null))
    invoke[Remote[mod.Peer] => AnyRef]("$loci$anonymous$23", Int.box(200))(dummyRemote) should (be (()) or be (null))
  }
}
