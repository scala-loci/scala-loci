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

  def assemblePeerImplementation(records: List[Any]): List[Any] = {
    val (placedValuesImpl, signatureImpl) = {
      // inherit implementation for placed values defined in the module bases
      val (placedValuesBases, moduleSignatureBases) =
        (module.tree.impl.parents collect {
          case parent if
              parent.tpe =:!= definitions.AnyTpe &&
              parent.tpe =:!= definitions.AnyRefTpe =>
            atPos(parent.pos) {
              tq"super[${parent.symbol.name.toTypeName}].${names.placedValues}"
            } ->
              q"super[${parent.symbol.name.toTypeName}].$$loci$$sig"
        }).unzip

      // collect placed values
      val placedValues = records collect {
        case PlacedValueDef(_, tree, _, _) => tree
      }

      // generate signature bases
      val signatureBases = moduleSignatureBases.foldRight[Tree](trees.nil) { (base, tree) =>
        q"$tree.::($base)"
      }

      val signatureImpl = module.outer match {
        case Some((value, outer)) =>
          q"${Flag.SYNTHETIC} protected def $$loci$$sig = ${trees.moduleSignature}(${uniqueRealisticName(module.symbol)}, $signatureBases, $outer.$$loci$$sig, $value)"
        case _ =>
          q"${Flag.SYNTHETIC} protected def $$loci$$sig = ${trees.moduleSignature}(${uniqueRealisticName(module.symbol)}, $signatureBases)"
      }

      // generate placed values
      val parents = tq"${types.placedValues}" :: placedValuesBases
      val placedValuesImpl = q"${Flag.SYNTHETIC} trait ${names.placedValues} extends ..$parents { ..$placedValues }"

      placedValuesImpl -> signatureImpl
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
        if (!module.symbol.isAbstract) {
          val system = q"${Flag.SYNTHETIC} def $$loci$$sys: ${types.system} = $$loci$$system"
          q"${Flag.SYNTHETIC} def ${name.toTermName}($$loci$$system: ${types.system}): ${names.placedValues} = new $name { $system }"
        }
        else
          q"${Flag.SYNTHETIC} def ${name.toTermName}($$loci$$system: ${types.system}): ${names.placedValues}"

      // generate peer signature
      val signatureBases = bases.foldRight[Tree](trees.nil) { (base, tree) =>
        val expr = base.tree match {
          case tq"$expr.$_" =>
            expr
          case _ =>
            val tq"$expr.$_" = createTypeTree(base.tpe.underlying, NoPosition)
            expr
        }
        q"$tree.::($expr.${TermName(s"$$loci$$peer$$sig$$${base.tpe.typeSymbol.name}")})"
      }
      val peerSignature =
        q"${Flag.SYNTHETIC} def ${TermName(s"$$loci$$peer$$sig$$${symbol.name}")} = ${trees.peerSignature}(${symbol.name.toString}, $signatureBases, $$loci$$sig)"

      Seq(peerImpl, peerValue, peerSignature)
    }

    // create records for new peer implementations
    val stats = (records collect { case value: ModuleValue => value.tree }) ++ (placedValuesImpl +: signatureImpl +: peerValues)

    // assemble multitier module
    val tree = module.tree map { (mods, parents, self, _) =>
      (mods mapAnnotations { multitierModuleAnnotation :: _ },
       parents,
       treeCopy.ValDef(self, self.mods, module.self, self.tpt, self.rhs),
       stats)
    }

    // add assembled results to records
    Assembly(castsInserter transform tree match { case tree: ImplDef => tree }) :: records
  }

  private object castsInserter extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case q"$recv[$tpt]($arg)" if recv.symbol == symbols.cast =>
        atPos(tree.pos) { q"$arg.asRemote[$tpt]" }
      case _ =>
        super.transform(tree)
    }
  }

  private val multitierModuleAnnotation =
    internal.setType(q"new ${types.multitierModule}", types.multitierModule)
}
