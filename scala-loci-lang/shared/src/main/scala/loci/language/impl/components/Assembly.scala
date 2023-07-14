package loci
package language
package impl
package components

import scala.reflect.macros.blackbox

object Assembly extends Component.Factory[Assembly](
    requires = Seq(Commons, ModuleInfo, Peers, Values, Initialization)) {
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
  val initialization = engine.require(Initialization)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._
  import peers._
  import values._
  import initialization._

  case class Assembly(tree: ImplDef)

  def assemblePeerImplementation(records: List[Any]): List[Any] = {
    logging.debug(" Assembling expanded multitier module")

    val moduleBody = records.collectFirst {
      case Initialized(tree) => tree.impl.body
    }.getOrElse(c.abort(NoPosition, "Did not find initialized module body"))

    val (placedValuesImpl, signatureImpl, moduleImpl, selfType) = {
      // inherit implementation for placed values defined in the module bases
      val placedValuesBases =
        module.tree.impl.parents collect {
          case parent if
              parent.tpe =:!= definitions.AnyTpe &&
              parent.tpe =:!= definitions.AnyRefTpe =>
            atPos(parent.pos) {
              tq"${names.placedValues(parent.symbol)}"
            }
        }

      // self-type annotations for implementation for placed values
      val selfType = {
        def selfType(tree: Tree): Tree = tree match {
          case tq"$tree with ..$trees" =>
            tq"${selfType(tree)} with ..${trees map selfType}"

          case _ if tree.tpe != NoType =>
            val tpe = tree.tpe match {
              case RefinedType(Seq(tpe0, tpe1), _)
                  if tpe0.typeSymbol == module.classSymbol =>
                tpe1
              case tpe =>
                tpe
            }
            tq"${names.placedValues(tpe.typeSymbol)}"

          case _ =>
            tq""
        }

        selfType(module.tree.impl.self.tpt)
      }

      // collect placed values
      val placedValues = records collect {
        case PlacedValueDef(_, tree, _, _) => tree
      }

      // generate signature
      val signatureImpl = (module.outer
        map { case (value, outer) =>
          q"""${Flag.SYNTHETIC} protected lazy val $$loci$$sig: ${types.moduleSignature} =
            ${trees.moduleSignature}($outer.$$loci$$sig, $value)"""
        }
        getOrElse {
          q"""${Flag.SYNTHETIC} protected lazy val $$loci$$sig: ${types.moduleSignature} =
            ${trees.moduleSignature}(${uniqueRealisticName(module.symbol)})"""
        })

      // generate module identifier
      val moduleImpl = q"""${Flag.SYNTHETIC} private lazy val $$loci$$mod = ${uniqueRealisticName(module.symbol)}"""

      // generate placed values
      val parents = placedValuesBases :+ tq"${types.placedValues}"
      val placedValuesImpl =
        q"""${Flag.SYNTHETIC} trait ${names.placedValues(module.symbol)} extends ..$parents {
          this: $selfType =>
          ..$placedValues
        }"""

      (placedValuesImpl, signatureImpl, moduleImpl, selfType)
    }

    val peerValues = getModulePeers(moduleBody) map { case Peer(symbol, name, bases, ties, instantiable) =>
      // inherit implementation for overridden peer types
      // i.e., types of the same name in the module base types
      val overriddenBases = module.tree.impl.parents flatMap { parent =>
        if ((parent.tpe member symbol.name) != NoSymbol)
          Some(atPos(parent.pos) {
            tq"${TypeName(s"$$loci$$peer$$${uniqueName(parent.symbol, symbol.name.toString)}")}"
          })
        else
          None
      }

      // inherit implementation for peer bases defined in the same module
      val inheritedBases = bases collect {
        case Peer.InheritedBase(tpe, tq"$expr.$_") =>
          val name = TypeName(s"$$loci$$peer$$${uniqueName(module.symbol, tpe.typeSymbol.name.toString)}")
          tq"$expr.$name"
        case Peer.InheritedBase(tpe, _) =>
          val name = TypeName(s"$$loci$$peer$$${uniqueName(module.symbol, tpe.typeSymbol.name.toString)}")
          val tq"$expr.$_" = createTypeTree(tpe.underlying, NoPosition): @unchecked
          tq"$expr.$name"
      }

      // collect values placed on the peer
      val placedValues = records collect {
        case PlacedValuePeerImpl(_, tree, `symbol`, _) => tree
      }

      // generate peer implementations
      val parents = overriddenBases ++ inheritedBases :+ tq"${names.placedValues(module.symbol)}"
      val peerImpl = if (instantiable) {
        q"""${Flag.SYNTHETIC} trait $name extends ..$parents {
          this: $selfType =>
          ..$placedValues
        }"""
      } else {
        q"""@$nonInstantiableAnnotation ${Flag.SYNTHETIC} trait $name extends ..$parents {
          this: $selfType =>
          ..$placedValues
        }"""
      }

      // generate peer signature
      val signatureBases = bases.foldRight[Tree](trees.nil) { (base, tree) =>
        val expr = base.tree match {
          case tq"$expr.$_" =>
            expr
          case _ =>
            val tq"$expr.$_" = createTypeTree(base.tpe.underlying, NoPosition): @unchecked
            expr
        }
        q"$tree.::($expr.${TermName(s"$$loci$$peer$$sig$$${base.tpe.typeSymbol.name}")})"
      }
      val peerSignature =
        q"""${Flag.SYNTHETIC} lazy val ${TermName(s"$$loci$$peer$$sig$$${symbol.name}")}: ${types.peerSignature} =
          ${trees.peerSignature}(${symbol.name.toString}, $signatureBases, $$loci$$sig)"""

      // generate peer tie specification
      val peerTies = ties map { tie =>
        val name = TermName(s"$$loci$$peer$$sig$$${tie.tpe.typeSymbol.name}")
        val expr = tie.tree match {
          case tq"$expr.$_" =>
            expr
          case _ =>
            val tq"$expr.$_" = createTypeTree(tie.tpe.underlying, NoPosition): @unchecked
            expr
        }

        tie.multiplicity match {
          case Tie.Multiple => q"($expr.$name, ${trees.multiple})"
          case Tie.Optional => q"($expr.$name, ${trees.optional})"
          case Tie.Single => q"($expr.$name, ${trees.single})"
        }
      }
      val peerTieSpec =
        q"${Flag.SYNTHETIC} def ${TermName(s"$$loci$$peer$$ties$$${symbol.name}")}: ${types.tieSignature} = ${trees.map}(..$peerTies)"

      Seq(peerImpl, peerSignature, peerTieSpec)
    }

    val (peerTypeBodySymbolTrees, moduleValues) = (records collect {
      case ModuleValue(symbol, tree @ TypeDef(mods, name, tparams, rhs))
          if checkPeerType(symbol).nonEmpty =>
        // collect super peers and tie specifications
        val (parents, body) = rhs.original match {
          case TypeBoundsTree(_, hi) => hi.original match {
            case CompoundTypeTree(Template(parents, _, body)) =>
              parents -> body
            case parent @ (Select(_, _) | Ident(_)) =>
              List(parent) -> List.empty
            case _ =>
              List.empty -> List.empty
          }
          case _ =>
            List.empty -> List.empty
        }

        // remove tie specification from peers defined inside the module
        // we outsource the tie specification into a separate synthetically generated trait
        // to work around http://github.com/scala/bug/issues/10928
        Some(symbol -> body) -> atPos(rhs.pos orElse tree.pos) {
          val tieName = TypeName(s"$$loci$$peer$$tie$$${uniqueName(module.symbol, tree.symbol.name.toString)}")
          val tieParents = tq"${types.peerMarker} with ..$parents with $tieName"
          val rhs = TypeBoundsTree(EmptyTree, tieParents)
          treeCopy.TypeDef(tree, mods, name, tparams, rhs)
        }

      case ModuleValue(_, tree) =>
        None -> tree
    }).unzip

    val peerTypeBodies = peerTypeBodySymbolTrees.flatten.toMap

    val peerTypeTieTrees = getModulePeers(moduleBody) map { peer =>
      // collect inherited synthetic peer traits
      val parents = (module.tree.impl.parents collect {
        case parent if
            parent.tpe =:!= definitions.AnyTpe &&
            parent.tpe =:!= definitions.AnyRefTpe =>
          if ((parent.tpe member peer.symbol.name) != NoSymbol)
            Some(atPos(parent.pos) {
              tq"${TypeName(s"$$loci$$peer$$tie$$${uniqueName(parent.symbol, peer.symbol.name.toString)}")}"
            })
          else
            None
      }).flatten

      val tieName = TypeName(s"$$loci$$peer$$tie$$${uniqueName(module.symbol, peer.symbol.name.toString)}")

      (peerTypeBodies get peer.symbol
        map { body =>
          // generate synthetic peer trait for tie specification
          // for peers defined inside the module
          val tiesSpecified = body exists {
            case TypeDef(_, TypeName("Tie"), _, _) => true
            case _ => false
          }

          val stats =
            if (tiesSpecified)
              body
            else
              q"type Tie" :: body

          Seq(q"${Flag.SYNTHETIC} trait $tieName extends ..$parents { ..$stats }")
        }
        getOrElse {
          // generate synthetic peer trait for tie specification
          // for inherited peers
          val peerParents = types.peerMarker :: (peer.bases map { _.tpe }) map {
            createTypeTree(_, c.enclosingPosition)
          }

          val ties = peer.ties map { tie =>
            tie.multiplicity match {
              case Tie.Single => types.single mapArgs { _ => List(tie.tpe) }
              case Tie.Optional => types.optional mapArgs { _ => List(tie.tpe) }
              case Tie.Multiple => types.multiple mapArgs { _ => List(tie.tpe) }
            }
          }

          val tie = ties map { createTypeTree(_, c.enclosingPosition) } match {
            case head :: tail => List(q"type Tie <: $head with ..$tail")
            case _ => List(q"type Tie")
          }

          Seq(
            q"@$peerAnnotation ${Flag.SYNTHETIC} type ${peer.symbol.name.toTypeName} <: ..$peerParents with $tieName",
            q"${Flag.SYNTHETIC} trait $tieName extends ..$parents { ..$tie }")
        })
    }

    // create records for new peer implementations
    val stats = moduleValues ++ peerTypeTieTrees.flatten ++ (placedValuesImpl +: signatureImpl +: moduleImpl +: peerValues.flatten)

    // assemble multitier module
    val tree = module.tree map { (mods, parents, self, _) =>
      (mods mapAnnotations { multitierModuleAnnotation :: _ },
       parents,
       treeCopy.ValDef(self, self.mods, module.self, self.tpt, self.rhs),
       stats)
    }

    // add assembled results to records
    Assembly((castsInserter transform tree: @unchecked) match { case tree: ImplDef => tree }) :: records
  }

  private object castsInserter extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case q"$recv[$tpt]($arg)" if recv.symbol == symbols.remoteCast =>
        atPos(tree.pos) { q"$arg.asRemote[$tpt]" }
      case q"$cast[..$tpts]($arg)" if cast.symbol == symbols.valueRefCast =>
        atPos(tree.pos) { q"$arg.at[${tpts.last}]" }
      case _ =>
        super.transform(tree)
    }
  }

  private val multitierModuleAnnotation =
    internal.setType(q"new ${types.multitierModule}", types.multitierModule)

  private val peerAnnotation =
    internal.setType(q"new ${types.peer}", types.peer)

  private val nonInstantiableAnnotation =
    internal.setType(q"new ${types.nonInstantiable}", types.nonInstantiable)
}
