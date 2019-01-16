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

  case class Assembly(tree: ImplDef)

  private def assemblePeerImplementation(records: List[Any]): List[Any] = {
    val placedValuesImpl = {
      // inherit implementation for placed values defined in the module bases
      val placedValuesBases = module.tree.impl.parents collect {
        case parent if
            parent.tpe =:!= definitions.AnyTpe &&
            parent.tpe =:!= definitions.AnyRefTpe =>
          atPos(parent.pos) {
            tq"super[${parent.symbol.name.toTypeName}].${names.placedValues}"
          }
      }

      // collect placed values
      val placedValues = records collect {
        case PlacedValueDef(_, tree, _, _) => tree
      }

      // generate placed values
      val parents = tq"${types.placedValues}" :: placedValuesBases
      q"${Flag.SYNTHETIC} trait ${names.placedValues} extends ..$parents { ..$placedValues }"
    }

    val peerValues = modulePeers flatMap { case Peer(symbol, name, bases, _) =>
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

      // generate peer implementations
      val parents = tq"${names.placedValues}" :: overriddenBases ++ inheritedBases
      val peerImpl = q"${Flag.SYNTHETIC} trait $name extends ..$parents { ..$placedValues }"

      // generate peer values
      val peerValue =
        if (module.symbol.isAbstract)
          q"${Flag.SYNTHETIC} def ${name.toTermName}: ${names.placedValues}"
        else
          q"${Flag.SYNTHETIC} def ${name.toTermName}: ${names.placedValues} = new $name { }"

      Seq(peerImpl, peerValue)
    }

    // create records for new peer implementations
    val stats = (records collect { case value: ModuleValue => value.tree }) ++ (placedValuesImpl +: peerValues)

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

  private val multitierModuleAnnotation =
    internal.setType(q"new ${types.multitierModule}", types.multitierModule)
}
