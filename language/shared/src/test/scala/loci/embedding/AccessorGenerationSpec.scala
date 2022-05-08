package loci
package embedding

import loci.language._
import loci.transmitter.IdenticallyTransmittable
import loci.transmitter.Serializable
import runtime.MarshallableValue

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import scala.util.Try

class AccessorGenerationSpec extends AnyFlatSpec with Matchers {
  behavior of "Accessor Generation"

  it should "defer accessor for deferred inherited marshallable value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0 extends DeferredSuperMod

    CompileTimeUtils.containsValueOfType[Mod0, MarshallableValue[_, _, _, _]] should be (false)
  }

  it should "create accessor for deferred inherited marshallable value" in {
    @multitier(AccessorGeneration.Preferred) trait Mod0 extends DeferredSuperMod
    @multitier(AccessorGeneration.Required) trait Mod1 extends DeferredSuperMod
    @multitier(AccessorGeneration.Forced) trait Mod2 extends DeferredSuperMod

    CompileTimeUtils.containsValueOfType[
      Mod0,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod1,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod2,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)
  }

  it should "create different accessor for inherited marshallable value" in {
    @multitier(AccessorGeneration.Forced) trait Mod3 extends ForcedSuperModByImport

    CompileTimeUtils.containsValueOfType[Mod3, MarshallableValue[_, _, _, _]] should be (true)
  }

  it should "use different inherited accessor for marshallable value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0 extends ForcedSuperModByImport
    @multitier(AccessorGeneration.Preferred) trait Mod1 extends ForcedSuperModByImport
    @multitier(AccessorGeneration.Required) trait Mod2 extends ForcedSuperModByImport
    @multitier(AccessorGeneration.Forced) trait Mod3 extends ForcedSuperModByImport

    CompileTimeUtils.containsValueOfType[Mod0, MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod1, MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod2, MarshallableValue[_, _, _, _]] should be (false)
  }

  it should "use same inherited accessor for marshallable value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0 extends ForcedSuperModByCompanion
    @multitier(AccessorGeneration.Preferred) trait Mod1 extends ForcedSuperModByCompanion
    @multitier(AccessorGeneration.Required) trait Mod2 extends ForcedSuperModByCompanion
    @multitier(AccessorGeneration.Forced) trait Mod3 extends ForcedSuperModByCompanion

    CompileTimeUtils.containsValueOfType[Mod0, MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod1, MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod2, MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod3, MarshallableValue[_, _, _, _]] should be (false)
  }

  it should "defer accessor for unused marshallable value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
    }

    CompileTimeUtils.containsValueOfType[Mod0, MarshallableValue[_, _, _, _]] should be (false)
  }

  it should "create accessor for unused marshallable value" in {
    @multitier(AccessorGeneration.Preferred) trait Mod0 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
    }

    @multitier(AccessorGeneration.Required) trait Mod1 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
    }

    @multitier(AccessorGeneration.Forced) trait Mod2 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
    }

    CompileTimeUtils.containsValueOfType[
      Mod0,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod1,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod2,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)
  }

  it should "create accessor for used marshallable value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
      on[Node] { data.asLocal }
    }

    @multitier(AccessorGeneration.Preferred) trait Mod1 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
      on[Node] { data.asLocal }
    }

    @multitier(AccessorGeneration.Required) trait Mod2 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
      on[Node] { data.asLocal }
    }

    @multitier(AccessorGeneration.Forced) trait Mod3 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: MarshallableData on Node = new MarshallableData
      on[Node] { data.asLocal }
    }

    CompileTimeUtils.containsValueOfType[
      Mod0,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod1,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod2,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)

    CompileTimeUtils.containsValueOfType[
      Mod3,
      MarshallableValue[MarshallableData, MarshallableData, MarshallableData, Future[MarshallableData]]] should be (true)
  }

  it should "defer accessor for unused non-marshallable concrete value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: NonMarshallableData on Node = new NonMarshallableData
    }

    @multitier(AccessorGeneration.Preferred) trait Mod1 {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: NonMarshallableData on Node = new NonMarshallableData
    }

    CompileTimeUtils.containsValueOfType[Mod0, MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod1, MarshallableValue[_, _, _, _]] should be (false)
  }

  it should "defer accessor for unused non-marshallable parametric value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0[T] {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: NonMarshallableParametricData[T] on Node = new NonMarshallableParametricData[T]
    }

    @multitier(AccessorGeneration.Preferred) trait Mod1[T] {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: NonMarshallableParametricData[T] on Node = new NonMarshallableParametricData[T]
    }

    CompileTimeUtils.containsValueOfType[Mod0[_], MarshallableValue[_, _, _, _]] should be (false)
    CompileTimeUtils.containsValueOfType[Mod1[_], MarshallableValue[_, _, _, _]] should be (false)
  }

  it should "defer accessor for used non-marshallable parametric value" in {
    @multitier(AccessorGeneration.Deferred) trait Mod0[T] {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: T on Node
      on[Node] { data.asLocal }
    }

    @multitier(AccessorGeneration.Preferred) trait Mod1[T] {
      @peer type Node <: { type Tie <: Single[Node] }
      val data: T on Node
      on[Node] { data.asLocal }
    }

    CompileTimeUtils.containsValueOfType[Mod0[_], MarshallableValue[_, _, _, _]] should be (true)
    CompileTimeUtils.containsValueOfType[Mod1[_], MarshallableValue[_, _, _, _]] should be (true)
  }

  it should "fail for accessor for unused non-marshallable concrete value" in {
    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Required) trait Mod {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableData on Node = new NonMarshallableData
      }
    }) should be (true)

    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Forced) trait Mod {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableData on Node = new NonMarshallableData
      }
    }) should be (true)
  }

  it should "fail for accessor for unused non-marshallable parametric value" in {
    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Required) trait Mod[T] {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableParametricData[T] on Node = new NonMarshallableParametricData[T]
      }
    }) should be (true)

    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Forced) trait Mod[T] {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableParametricData[T] on Node = new NonMarshallableParametricData[T]
      }
    }) should be (true)
  }

  it should "fail for accessor for used non-marshallable concrete value" in {
    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Deferred) trait Mod {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableData on Node = new NonMarshallableData
        on[Node] { data.asLocal }
      }
    }) should be (true)

    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Preferred) trait Mod {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableData on Node = new NonMarshallableData
        on[Node] { data.asLocal }
      }
    }) should be (true)

    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Required) trait Mod {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableData on Node = new NonMarshallableData
        on[Node] { data.asLocal }
      }
    }) should be (true)

    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Forced) trait Mod {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: NonMarshallableData on Node = new NonMarshallableData
        on[Node] { data.asLocal }
      }
    }) should be (true)
  }

  it should "fail for accessor for used non-marshallable parametric value" in {
    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Required) trait Mod2[T] {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: T on Node
        on[Node] { data.asLocal }
      }
    }) should be (true)

    CompileTimeUtils.containsCompileTimeOnly({
      @multitier(AccessorGeneration.Forced) trait Mod3[T] {
        @peer type Node <: { type Tie <: Single[Node] }
        val data: T on Node
        on[Node] { data.asLocal }
      }
    }) should be (true)
  }
}

class NonMarshallableData

class NonMarshallableParametricData[T]

class MarshallableData

object MarshallableData {
  implicit def transmittable: IdenticallyTransmittable[MarshallableData] = IdenticallyTransmittable()
  implicit def serializable: Serializable[MarshallableData] = new Serializable[MarshallableData] {
    def serialize(value: MarshallableData) = MessageBuffer.empty
    def deserialize(value: MessageBuffer) = Try { new MarshallableData }
  }
}

@multitier(AccessorGeneration.Deferred) trait DeferredSuperMod {
  @peer type Node <: { type Tie <: Single[Node] }
  val data: MarshallableData on Node = new MarshallableData
}

@multitier(AccessorGeneration.Forced) trait ForcedSuperModByCompanion {
  @peer type Node <: { type Tie <: Single[Node] }
  val data: MarshallableData on Node = new MarshallableData
}

object MarshallableDataDefintions {
  implicit def transmittable: IdenticallyTransmittable[MarshallableData] = IdenticallyTransmittable()
  implicit def serializable: Serializable[MarshallableData] = new Serializable[MarshallableData] {
    def serialize(value: MarshallableData) = MessageBuffer.empty
    def deserialize(value: MessageBuffer) = Try { new MarshallableData }
  }
}

import MarshallableDataDefintions._

@multitier(AccessorGeneration.Forced) trait ForcedSuperModByImport {
  @peer type Node <: { type Tie <: Single[Node] }
  val data: MarshallableData on Node = new MarshallableData
}
