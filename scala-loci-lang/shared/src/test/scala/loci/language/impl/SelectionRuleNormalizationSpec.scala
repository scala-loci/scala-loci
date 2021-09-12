package loci
package language
package impl

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class SelectionRuleNormalizationSpec extends AnyFlatSpec with Matchers with NoLogging {
  behavior of "Selection rule normalization"

  it should "compile for parameterless selection rule" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }

      def select: Local[Remote[Node]] on Node = on[Node] { implicit! =>
        remote[Node].connected.head
      }

      def f: Int on Node = on[Node] { 42 }

      def remoteExecutions(): Unit on Node = on[Node] { implicit! =>
        onAny.apply[Node](select).run { implicit! => f }
        onAny.apply[Node](select).run.sbj { implicit! => _: Remote[Node] => f }
        remoteAny.apply[Node](select).call(f)
        onAny.apply[Node](select).run { implicit! => 42 }.asLocal
        onAny.apply[Node](select).run.sbj { implicit! => _: Remote[Node] => f }.asLocal
        remoteAny.apply[Node](select).call(f).asLocal
      }
    }""" should compile
  }

  it should "compile for selection rule on connected remotes" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }

      def select(connected: Seq[Remote[Node]]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
        connected.head
      }

      def f: Int on Node = on[Node] { 42 }

      def remoteExecutions(): Unit on Node = on[Node] { implicit! =>
        onAny.apply[Node](select _).run { implicit! => f }
        onAny.apply[Node](select _).run.sbj { implicit! => _: Remote[Node] => f }
        remoteAny.apply[Node](select _).call(f)
        onAny.apply[Node](select _).run { implicit! => 42 }.asLocal
        onAny.apply[Node](select _).run.sbj { implicit! => _: Remote[Node] => f }.asLocal
        remoteAny.apply[Node](select _).call(f).asLocal
      }
    }""" should compile
  }

  it should "compile for selection rule on connected remotes and self reference" in {
    """@multitier object Module {
      @peer type Node <: { type Tie <: Multiple[Node] }

      def select(connected: Seq[Remote[Node]], self: SelfReference[Node]): Local[Remote[Node]] on Node = on[Node] { implicit! =>
        self
      }

      def f: Int on Node = on[Node] { 42 }

      def remoteExecutions(): Unit on Node = on[Node] { implicit! =>
        onAny.apply[Node](select _).run { implicit! => f }
        onAny.apply[Node](select _).run.sbj { implicit! => _: Remote[Node] => f }
        remoteAny.apply[Node](select _).call(f)
        onAny.apply[Node](select _).run { implicit! => 42 }.asLocal
        onAny.apply[Node](select _).run.sbj { implicit! => _: Remote[Node] => f }.asLocal
        remoteAny.apply[Node](select _).call(f).asLocal
      }
    }""" should compile
  }
}
