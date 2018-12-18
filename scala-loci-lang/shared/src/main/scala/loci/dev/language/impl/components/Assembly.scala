package loci.dev
package language
package impl
package components

import scala.reflect.macros.blackbox

object Assembly extends Component.Factory[Assembly](
    requires = Seq(Commons, ModuleInfo, Peers, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Assembly(engine)
  def asInstance[C <: blackbox.Context] = { case c: Assembly[C] => c }
}

class Assembly[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("assembly", assemblePeerImplementation, after = Set("*", "values:fixrefs")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)
  val peers = engine.require(Peers)
  val values = engine.require(Values)

  import engine.c.universe._
  import commons._
  import moduleInfo._
  import peers._
  import values._

  case class Assembly(tree: Tree)

  private def assemblePeerImplementation(records: List[Any]): List[Any] = {
    val (placedValuesIdent, placedValues) = {
      // inherit implementation for placed values defined in the module bases
      val placedValuesBases = module.tree.impl.parents flatMap { base =>
        if ((base.tpe member names.placedValues) != NoSymbol)
          Some(tq"super[${base.symbol.name.toTypeName}].${names.placedValues}")
        else
          None
      }

      // collect placed values
      val placedValues = records collect {
        case PlacedValueDef(_, tree, _, _) => tree
      }

      // we only need a trait for placed values if we have placed values or
      // we inherit more than one trait for placed values (which need to be merged)
      if (placedValues.nonEmpty || placedValuesBases.size > 1)
        Some(tq"${names.placedValues}") ->
          Some(q"${Flag.SYNTHETIC} trait ${names.placedValues} extends ..${trees.placedValues :: placedValuesBases} { ..$placedValues }")
      else if (placedValuesBases.size == 1)
        Some(tq"${names.placedValues}") -> None
      else
        None -> None
    }

    val peerValues = modulePeers map { case Peer(symbol, name, bases, _) =>
      // inherit implementation for overridden peer types
      // i.e., types of the same name in the module base types
      val overriddenBases = module.tree.impl.parents flatMap { base =>
        if ((base.tpe member symbol.name) != NoSymbol)
          Some(tq"super[${base.symbol.name.toTypeName}].$name")
        else
          None
      }

      // inherit implementation for peer bases defined in the same module
      val inheritedBases = bases collect {
        case Peer.InheritedBase(_, name, tree) if !tree.isEmpty => tq"$name"
      }

      // collect values placed on the peer
      val placedValues = records collect {
        case PlacedValuePeerImpl(_, tree, `symbol`, _) => tree
      }

      // generate peer values
      q"${Flag.SYNTHETIC} trait $name extends ..${placedValuesIdent ++ overriddenBases ++ inheritedBases} { ..$placedValues }"
    }

    // create records for new peer implementations
    val stats = (records collect { case value: ModuleValue => value.tree }) ++ placedValues ++ peerValues

    // assemble multitier module
    val tree = module.tree map { (mods, parents, self, _) =>
      (mods mapAnnotations { multitierModuleAnnotation :: _ },
       parents,
       treeCopy.ValDef(self, self.mods, names.multitierModule, self.tpt, self.rhs),
       stats)
    }

    // add assembled results to records
    Assembly(tree) :: records
  }

  private val multitierModuleAnnotation = q"new ${trees.multitierModule}"
}
