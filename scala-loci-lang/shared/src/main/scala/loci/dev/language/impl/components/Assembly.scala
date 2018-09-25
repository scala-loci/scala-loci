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
    val values = records collect { case value: Value => value }

    val (nonplaced, nonplacedImpl) = {
      // inherit implementation for non-placed values defined in the module bases
      val nonplacedBases = module.tree.impl.parents flatMap { base =>
        val basePeer = base.tpe member names.placedValues
        if (basePeer != NoSymbol)
          Some(tq"super[${base.symbol.name.toTypeName}].${names.placedValues}")
        else
          None
      }

      val nonplacedValues = values collect {
        case NonPlacedValue(_, _, tree) => tree
      }

      // we only need a trait for non-placed values if we have non-placed values or
      // we inherit more than one trait for non-placed values (which needs to be merged)
      if (nonplacedValues.nonEmpty || nonplacedBases.size > 1)
        Some(tq"${names.placedValues}") ->
          Some(q"${Flag.SYNTHETIC} trait ${names.placedValues} extends ..${trees.placedValues :: nonplacedBases} { ..$nonplacedValues }")
      else
        None -> None
    }

    val peerImpls = modulePeers flatMap { case peer @ Peer(symbol, name, bases, _) =>
      // inherit implementation for overridden peer types
      // i.e., types of the same name in the module base types
      val overriddenBases = module.tree.impl.parents flatMap { base =>
        val basePeer = base.tpe member symbol.name
        if (basePeer != NoSymbol)
          Some(tq"super[${base.symbol.name.toTypeName}].$name")
        else
          None
      }

      // inherit implementation for peer bases defined in the same module
      val inheritedBases = bases collect {
        case Peer.InheritedBase(_, name, tree) if !tree.isEmpty => tq"$name"
      }

      // delegate implementation for peer bases defined in a different module
      val delegatedBases = bases collect {
        case Peer.DelegatedBase(tpe, _, name, tree)
            if !tree.isEmpty && (module.symbol.info member name) == NoSymbol =>
          val peer = requirePeerType(tpe.typeSymbol)

          tree match {
            case tq"$ref.$_[..$_]" =>
              q"final val $name: $ref.${peer.name} = $ref.${peer.name.toTermName}"
            case tq"$tpname" =>
              q"final val $name: ${peer.name} = ${peer.name.toTermName}"
          }
      }

      // collect values placed on the peer
      val placedValues = values collect {
        case PlacedValue(_, _, tree, `symbol`, _) => tree
      }

      // generate peer implementation
      val peerBases = nonplaced ++ overriddenBases ++ inheritedBases
      val peerBody = delegatedBases ++ placedValues
      val construction = name.toTermName

      val peerImpl =
        q"${Flag.SYNTHETIC} trait $name extends ..$peerBases { ..$peerBody }"

      val peerConstruction =
        if (module.symbol.isAbstract)
          q"${Flag.SYNTHETIC} def $construction: $name"
        else
          q"${Flag.SYNTHETIC} def $construction: $name = new $name { }"

      Seq(peerImpl, peerConstruction)
    }

    // create records for new peer implementations
    val extendedRecords =
      records ++ (nonplacedImpl ++ peerImpls map { GlobalValue(NoSymbol, module.symbol, _) })

    // assemble multitier module
    val tree = module.tree map { (mods, parents, self, _) =>
      (mods mapAnnotations { multitierModuleAnnotation :: _ },
       parents,
       treeCopy.ValDef(self, self.mods, names.multitierModule, self.tpt, self.rhs),
       extendedRecords collect { case value: GlobalValue => value.tree })
    }

    // add assembled results to records
    Assembly(tree) :: records
  }

  private val multitierModuleAnnotation = q"new ${trees.multitierModule}"
}
