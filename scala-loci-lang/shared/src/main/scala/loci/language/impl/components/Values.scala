package loci
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Values extends Component.Factory[Values](
    requires = Seq(Commons, ModuleInfo, Initialization, Peers)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Values(engine)
  def asInstance[C <: blackbox.Context] = { case c: Values[C] => c }
}

class Values[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("values:collect", collectPlacedValues, after = Set("init:inst"), before = Set("*", "values:validate")),
    Phase("values:lift", processLiftedPlacedValues, after = Set("values:collect"), before = Set("*", "values:validate")),
    Phase("values:validate", validatePlacedValues, after = Set("*")),
    Phase("values:fixrefs", fixEnclosingReferences, after = Set("*", "values:validate")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)
  val initialization = engine.require(Initialization)
  val peers = engine.require(Peers)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._
  import initialization._
  import peers._


  sealed trait Modality

  object Modality {
    case object None extends Modality
    case object Local extends Modality
    case class Subjective(peer: Type) extends Modality
  }

  sealed trait Placement

  case class Placed(peer: Symbol, tpe: Type, tpt: Tree, modality: Modality) extends Placement

  case object NonPlaced extends Placement

  sealed trait Value {
    val symbol: Symbol
    val tree: Tree

    def copy(symbol: Symbol = symbol, tree: Tree = tree): Value = this match {
      case PlacedValueDef(_, _, peer, modality) => PlacedValueDef(symbol, tree, peer, modality)
      case PlacedValuePeerImpl(_, _, peer, modality) => PlacedValuePeerImpl(symbol, tree, peer, modality)
      case ModuleValue(_, _) => ModuleValue(symbol, tree)
    }
  }

  object Value {
    def unapply(value: Value) = Some((value.symbol, value.tree))
  }

  sealed trait PlacedValue extends Value {
    override def copy(symbol: Symbol = symbol, tree: Tree = tree): PlacedValue = this match {
      case PlacedValueDef(_, _, peer, modality) => PlacedValueDef(symbol, tree, peer, modality)
      case PlacedValuePeerImpl(_, _, peer, modality) => PlacedValuePeerImpl(symbol, tree, peer, modality)
    }
  }

  object PlacedValue {
    def unapply(value: PlacedValue) = value match {
      case PlacedValueDef(symbol, tree, peer, modality) =>
        Some((symbol, tree, peer, modality))
      case PlacedValuePeerImpl(symbol, tree, peer, modality) =>
        Some((symbol, tree, Some(peer), modality))
    }
  }

  case class PlacedValueDef(symbol: Symbol, tree: Tree,
    peer: Option[Symbol], modality: Modality) extends PlacedValue

  case class PlacedValuePeerImpl(symbol: Symbol, tree: Tree,
    peer: Symbol, modality: Modality) extends PlacedValue

  case class ModuleValue(symbol: Symbol, tree: Tree) extends Value


  // split statements into module-level and peer-level statements
  def collectPlacedValues(records: List[Any]): List[Any] = {
    val expr = s"$$loci$$expr$$${uniqueName(module.symbol)}$$"
    var index = 0

    // create a member for initializing a multitier module reference
    // at the level of placed values for every peer
    def setupMultitierModule(tree: Tree, multitierName: TermName, multitierType: Tree, signature: String): Seq[Value] =
      modulePeers map { peer =>
        val bases = multitierType :: (peer.bases collect {
          case Peer.DelegatedBase(TypeRef(pre, sym, _), _)
              if pre.termSymbol == tree.symbol =>
            val name = TypeName(s"$$loci$$peer$$${uniqueName(pre.termSymbol, sym.name.toString)}")
            tq"${module.self}.${pre.termSymbol.asTerm.name}.$name"
        })

        val system = q"${Flag.SYNTHETIC} protected def $$loci$$sys$$create: ${types.system} = ${peer.name}.this.$$loci$$sys"
        val instance = q"new ..$bases { $system }"
        val value = extractValue(multitierName, NoType, multitierType, instance, tree.pos)

        PlacedValuePeerImpl(tree.symbol, value, peer.symbol, Modality.None)
      }

    // rename references for a given symbol to a given name
    def rename(tree: Tree, symbol: Symbol, classSymbol: Symbol, name: TypeName) = {
      object transformer extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case tree: TypeTree if tree.original != null =>
            internal.setOriginal(TypeTree(), transform(tree.original))
          case This(_) if tree.symbol == classSymbol =>
            treeCopy.This(tree, name)
          case Ident(_) if tree.isType && tree.symbol == classSymbol =>
            treeCopy.Ident(tree, name)
          case Ident(_) if tree.isTerm && tree.symbol == symbol && symbol != NoSymbol =>
            treeCopy.Ident(tree, name.toTermName)
          case Select(qualifier, _) if tree.isType && tree.symbol == classSymbol =>
            super.transform(treeCopy.Select(tree, qualifier, name))
          case Select(qualifier, _) if tree.isTerm && tree.symbol == symbol && symbol != NoSymbol =>
            super.transform(treeCopy.Select(tree, qualifier, name.toTermName))
          case _ =>
            super.transform(tree)
        }
      }

      transformer transform tree
    }

    // remove the multitier abstract value annotation from the given defintion
    def removeAbstractValueAnnotation(tree: ValOrDefDef): ValOrDefDef = {
      internal.setAnnotations(
        tree.symbol,
        tree.symbol.annotations filterNot { _.tree.tpe <:< types.abstractValue }: _*)

      tree map { (mods, name, tpt, rhs) =>
        (mods mapAnnotations { _ filterNot { _.tpe <:< types.abstractValue } }, name, tpt, rhs)
      }
    }

    object abstractValueAnnotationRemover extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: ImplDef if !isMultitierModule(tree.symbol.info, tree.pos) =>
          val body = tree.impl.body map {
            case tree: ValOrDefDef
                if tree.mods.annotations exists { _.tpe <:< types.abstractValue } =>
              removeAbstractValueAnnotation(tree map { (mods, name, tpt, rhs) =>
                (mods withFlags Flag.DEFERRED, name, tpt, EmptyTree)
              })

            case tree =>
              tree
          }

          super.transform(
            tree map { (mods, parents, self, _) => (mods, parents, self, body) })

        case _ =>
          super.transform(tree)
      }
    }

    // create module-level members and corresponding members at the level of placed values
    // for multitier module implementations
    // 1) keep the member at the module-level,
    //    but rename it and create a value alias to make the compiler happy
    // 2) create a reference to the multitier module's placed values at the level of placed values and
    // 3) (potentially) create an initializing member at the level of placed values for every peer
    def splitMultitierModule(tree: ModuleDef): Seq[Value] = {
      val multitierName = TermName(s"$$loci$$multitier$$${tree.name}")
      val multitierType = tq"${module.self}.${tree.name}.${names.placedValues(tree.symbol)}"

      val liftedMods = liftMods(tree.symbol, tree.mods)
      val mods = Modifiers(
        Flag.FINAL | Flag.LAZY | (if (liftedMods hasFlag Flag.PROTECTED) Flag.PROTECTED else NoFlags),
        liftedMods.privateWithin)

      val system = q"${Flag.SYNTHETIC} protected def $$loci$$sys$$create: ${types.system} = ${names.placedValues(module.symbol)}.this.$$loci$$sys"
      val instance = q"new $multitierType { $system }"
      val value = extractValue(multitierName, NoType, multitierType, instance, tree.pos)
      val application = atPos(tree.pos) { q"$mods val ${tree.name}: $multitierType = $multitierName()" }
      val reference = atPos(tree.pos) { q"$mods val ${tree.name}: $multitierName.type = $multitierName" }

      val definition = rename(
        treeCopy.ModuleDef(tree, liftedMods, multitierName, tree.impl),
        tree.symbol,
        tree.symbol.asModule.moduleClass,
        multitierName.toTypeName)

      Seq(
        PlacedValueDef(tree.symbol, application, None, Modality.None),
        PlacedValueDef(tree.symbol, value, None, Modality.None),
        ModuleValue(tree.symbol, definition),
        ModuleValue(tree.symbol, reference)) ++
      setupMultitierModule(tree, multitierName, multitierType, tree.name.toString)
    }

    // keep companion classes of multitier module implementations at the module-level,
    // but rename it and create a type alias to make the compiler happy
    // (like we do for the multitier module implementations themselves)
    def splitMultitierModuleCompanionClass(tree: ClassDef): Seq[Value] = {
      val multitierName = TypeName(s"$$loci$$multitier$$${tree.name}")

      val liftedMods = liftMods(tree.symbol, tree.mods)
      val mods = Modifiers(
        Flag.FINAL | (if (liftedMods hasFlag Flag.PROTECTED) Flag.PROTECTED else NoFlags),
        liftedMods.privateWithin)

      val reference = atPos(tree.pos) {
        q"$mods type ${tree.name} = $multitierName"
      }

      val definition = rename(
        treeCopy.ClassDef(tree, liftedMods, multitierName, tree.tparams, tree.impl),
        NoSymbol,
        tree.symbol,
        multitierName)

      Seq(
        ModuleValue(tree.symbol, definition),
        ModuleValue(tree.symbol, reference))
    }

    // create module-level members and corresponding members at the level of placed values
    // for value and method definitions
    def splitValOrDefDef(
        tree: ValOrDefDef)(
        concreteValues: (ValOrDefDef, Symbol, Type, Tree, Modality, Seq[(Tree, Symbol)]) => Seq[PlacedValue]): Seq[Value] =
      // for non-multitier references, create
      // 1) dummy member at the module-level, which is compile-time-only,
      // 2) member at the level of placed values
      // 3) (potentially) a peer-specific initializing member at the level of placed values,
      //    in which case the member (2) is always non-abstract
      if (!isMultitierModule(tree.tpt.tpe, tree.pos)) {
        val values = decomposePlacementType(tree.symbol.info, tree.tpt, tree.symbol, tree.pos, moduleDefinition = true) match {
          case Placed(peer, tpe, tpt, modality) =>
            if (isMultitierModule(tpe, tree.pos))
              c.abort(tree.pos, "Multitier module instances cannot be placed")

            val valOrDefDef = (changeType(tree, tpe, tpt)
              map { (mods, name, tpt, rhs) =>
                // add `override` modifier to synthesized value if necessary
                // since we initialize synthesized values with `null`
                // instead of keeping them abstract
                val liftedMods = liftMods(tree.symbol, mods)
                if (tree.symbol.overrides.isEmpty)
                  (liftedMods, name, tpt, rhs)
                else
                  (liftedMods withFlags Flag.OVERRIDE, name, tpt, rhs)
              })

            if (tree.symbol.isAbstract ||
                (tree.symbol.allAnnotations exists { _.tree.tpe <:< types.abstractValue })) {
              // initialize synthesized values with `null`
              // instead of keeping them abstract
              val erasure = stripPlacementSyntax(valOrDefDef) map { (mods, name, tpt, _) =>
                (mods withoutFlags Flag.DEFERRED, name, tpt, q"null.asInstanceOf[$tpt]")
              }
              Seq(PlacedValueDef(tree.symbol, removeAbstractValueAnnotation(erasure), Some(peer), modality))
            }
            else if (!tree.symbol.isAbstract &&
                     !(tree.symbol.allAnnotations exists { _.tree.tpe <:< types.abstractValue }) &&
                     (valOrDefDef.mods hasFlag Flag.MUTABLE) &&
                     valOrDefDef.rhs.isEmpty) {
              Seq(PlacedValueDef(tree.symbol, valOrDefDef, Some(peer), Modality.None))
            }
            else {
              // collect a list of peer-specific specialized implementations
              val specializations = splitPlacementSyntax(valOrDefDef.rhs) map { tree =>
                decomposePlacementType(tree.tpe, EmptyTree, valOrDefDef.symbol, tree.pos, moduleDefinition = true) match {
                  case Placed(peer, _, _, modality) =>
                    (stripPlacementSyntax(tree), peer, modality)
                  case _ =>
                    (tree, peer, modality)
                }
              }

              // ensure that peer-specific specialized implementations agree on their modalities
              specializations sliding 2 foreach {
                case Seq((_, _, modality0), (_, _, modality1)) =>
                  val equal = (modality0, modality1) match {
                    case (Modality.Subjective(peer1), Modality.Subjective(peer2)) =>
                      peer1 =:= peer2
                    case (Modality.Local, Modality.Local) | (Modality.None, Modality.None) =>
                      true
                    case _ =>
                      false
                  }

                  if (!equal)
                    c.abort(tree.pos, "Placed definitions must agree on subjective and local modalities")

                case _ =>
              }

              // ensure that peer-specific specialized implementations exist
              // 1) for the common super peer
              // 2) with at most one implementation per peer
              val peers = mutable.Set.empty[Symbol]

              specializations foreach { case (tree, peer, _) =>
                if (!peers.add(peer))
                  c.abort(tree.pos, s"Duplicate implementation for ${peer.name}")
              }

              val inferredUnit =
                if (!(peers contains peer))
                  (modality, tpe.underlying.typeArgs) match {
                    case (Modality.Subjective(_), Seq(peerType, argType)) if argType =:= definitions.UnitTpe =>
                      List((q"{ _: ${createTypeTree(peerType, tree.pos)} => () }", peer, modality))
                    case _ if tpe =:= definitions.UnitTpe =>
                      List((q"()", peer, modality))
                    case _ =>
                      c.abort(tree.pos, s"Missing implementation for common super peer ${peer.name}")
                  }
                else
                  List.empty

              // collect common and specialized implementations
              val (Seq((common, _, _)), specialized) = inferredUnit ++ specializations partition {
                case (_, `peer`, _) => true
                case _ => false
              }: @unchecked

              concreteValues(
                valOrDefDef map { (mods, name, tpt, _) => (mods, name, tpt, common) },
                peer, tpe, tpt, modality,
                specialized map { case (tree, peer, _) => tree -> peer })
            }

          case _ =>
            Seq(PlacedValueDef(tree.symbol, removeAbstractValueAnnotation(tree), None, Modality.None))
        }

        values :+ ModuleValue(tree.symbol, eraseValue(tree))
      }
      // for multitier references,
      // 1) keep the member at the module-level,
      // 2) create a reference to the multitier module's placed values at the level of placed values and
      // 3) (potentially) create an initializing member at the level of placed values for every peer
      else tree match {
        case _: ValDef =>
          val multitierName = TermName(s"$$loci$$multitier$$${tree.name}")
          val multitierType = tq"${module.self}.${tree.name}.${names.placedValues(tree.tpt.symbol)}"

          if (tree.symbol.isAbstract ||
              (tree.symbol.allAnnotations exists { _.tree.tpe <:< types.abstractValue })) {
            val application = tree map { (mods, name, _, _) =>
              (liftMods(tree.symbol, mods) withFlags Flag.DEFERRED, name, multitierType, EmptyTree)
            }

            val valDef = tree map { (mods, name, tpt, _) =>
              (mods withFlags Flag.DEFERRED, name, tpt, EmptyTree)
            }

            Seq(
              PlacedValueDef(tree.symbol, removeAbstractValueAnnotation(application), None, Modality.None),
              ModuleValue(tree.symbol, removeAbstractValueAnnotation(valDef)))
          }
          else {
            multitierInitializations get tree.symbol foreach { initializations =>
              if (initializations.head.isTerm)
                c.abort(tree.pos, s"Multitier module instance may not alias another value: ${initializations.head}")
              else if (initializations exists { _ != tree.tpt.symbol }) {
                initializations foreach { initialization =>
                  val name = initialization.toString
                  if (name startsWith "<$anon: ")
                    c.abort(initialization.pos orElse tree.pos,
                      "Multitier module instance may not be an instance of an anonymous class: " +
                      name.substring(8, name.length - 1))
                }
                c.abort(tree.pos, "Multitier module instance must have most precise type " +
                  s"of ${initializations.distinct mkString ", "}")
              }
            }

            // by ensuring that multitier module instances are always given their precise type (above)
            // we never should have to instantiate an abstract multitier module
            // except the type system was circumvented
            val (init, multitierModuleSetups) =
              if (!tree.tpt.symbol.isAbstract) {
                val system = q"${Flag.SYNTHETIC} protected def $$loci$$sys$$create: ${types.system} = ${names.placedValues(module.symbol)}.this.$$loci$$sys"
                q"new $multitierType { $system }" -> setupMultitierModule(tree, multitierName, multitierType, tree.name.toString)
              }
              else
                q"null.asInstanceOf[$multitierType]" -> Seq.empty

            val value = extractValue(multitierName, NoType, multitierType, init, tree.pos)
            val application = tree map { (mods, name, _, _) =>
              (liftMods(tree.symbol, mods), name, multitierType, q"$multitierName()")
            }

            Seq(
              PlacedValueDef(tree.symbol, application, None, Modality.None),
              PlacedValueDef(tree.symbol, value, None, Modality.None),
              ModuleValue(tree.symbol, tree)) ++
            multitierModuleSetups
          }

        case _ =>
          c.abort(tree.pos, s"Stable identifier required for multitier module instance: ${tree.symbol.name}")
      }

    // erase statements and expressions initializing a value from the module-level
    // to make them peer-specific
    // 1) create a no-op erased dummy value,
    // 2) create an overriding peer-specific "real" value and
    // 3) create an application of the value,
    //    which might be executing a statement or initializing a member
    def erase(
        tree: Tree, symbol: Symbol, peer: Symbol, tpe: Type, tpt: Tree, modality: Modality,
        specialized: Seq[(Tree, Symbol)],
        apply: Tree => Tree): Seq[PlacedValue] = {
      val exprName = TermName(s"$expr$index")
      index += 1

      val pos = tree.pos
      val erasure = eraseValue(exprName, tpe, tpt, tree.pos)
      val application = apply(q"$exprName()")
      val values = ((tree -> peer) +: specialized) map { case (tree, peer) =>
        val value = extractValue(exprName, tpe, tpt, tree, pos)
        PlacedValuePeerImpl(symbol, value, peer, modality)
      }

      values ++ Seq(
        PlacedValueDef(symbol, erasure, Some(peer), modality),
        PlacedValueDef(symbol, application, Some(peer), modality))
    }

    // create concrete multitier stub values for inherited abstract values
    // if the module is final
    val concreteStubs: List[ModuleValue] =
      if (module.symbol.isModule || module.symbol.isFinal)
        (module.symbol.info.members collect {
          case symbol
              if symbol.isMethod &&
                 symbol.isAbstract &&
                 symbol.owner != module.classSymbol &&
                 !isMultitierModule(symbol.info, symbol.pos) =>
            val method = symbol.asMethod
            val info = method.info.asSeenFrom(module.classSymbol)
            val result = createTypeTree(info.finalResultType, c.enclosingPosition)

            def hasPrivateWithinOwner(symbol: Symbol, privateWithin: Symbol): Boolean =
              symbol != NoSymbol &&
              (symbol == privateWithin ||
               symbol.name != privateWithin.name && hasPrivateWithinOwner(symbol.owner, privateWithin))

            if (method.privateWithin == NoSymbol ||
                hasPrivateWithinOwner(module.classSymbol, method.privateWithin)) {
              val flags =
                Flag.SYNTHETIC |
                (if (method.isPrivate) Flag.PRIVATE else NoFlags) |
                (if (method.isProtected) Flag.PROTECTED else NoFlags) |
                (if (method.isPrivateThis || method.isProtectedThis) Flag.LOCAL else NoFlags)
              val privateWithin =
                if (method.privateWithin == NoSymbol)
                  typeNames.EMPTY
                else
                  method.privateWithin.name
              val annotations =
                abstractValueAnnotation :: (symbol.annotations map { _.tree })
              val mods =
                Modifiers(flags, privateWithin, annotations)

              val tree =
                if (!method.isStable) {
                  val arguments = method.paramLists map {
                    _ map { symbol =>
                      val info = symbol.info.asSeenFrom(module.classSymbol)
                      val param = createTypeTree(info, c.enclosingPosition)
                      internal.setSymbol(
                        q"${Modifiers(Flag.PARAM)} val ${symbol.name.toTermName}: $param",
                        symbol)
                    }
                  }

                  q"$mods def ${method.name}(...$arguments): $result = null.asInstanceOf[$result]"
                }
                else
                  q"$mods val ${method.name}: $result = null.asInstanceOf[$result]"

              Some(ModuleValue(method, internal.setSymbol(tree, method)))
            }
            else
              None
        }).flatten.toList
      else
        List.empty

    // create module-level and peer-level values (including general placed values and peer-specific ones)
    // for all statements in the multitier module
    records ++ (records flatProcess {
      case Initialized(tree) =>
        tree.impl.body map abstractValueAnnotationRemover.transform flatMap {
          case tree: DefDef
            if tree.symbol.isTerm &&
               !tree.symbol.isConstructor &&
               (!tree.symbol.isSynthetic || !tree.symbol.asTerm.isParamWithDefault) =>
            logging.debug(s" Collecting ${tree.symbol}")

            splitValOrDefDef(tree) { (tree, peer, _, _, modality, specialized) =>
              PlacedValueDef(tree.symbol, tree, Some(peer), modality) +:
              (specialized map { case (rhs, peer) =>
                val specialized = tree map { (mods, name, tpt, _) =>
                  (mods withFlags Flag.OVERRIDE, name, tpt, rhs)
                }
                PlacedValuePeerImpl(tree.symbol, specialized, peer, modality)
              })
            }

          case tree: DefDef if tree.symbol.isConstructor =>
            logging.debug(s" Collecting ${tree.symbol}")

            tree.vparamss foreach {
              _ foreach { tree =>
                if (!(tree.mods hasFlag Flag.BYNAMEPARAM) && !(tree.mods hasFlag Flag.IMPLICIT))
                  c.abort(tree.pos, "Multitier module arguments must be call-by-name or implicit")
              }
            }

            Seq(ModuleValue(tree.symbol, tree))

          case tree: ValDef if tree.symbol.asTerm.isParamAccessor =>
            logging.debug(s" Collecting parameter ${tree.name}")

            if (!(tree.mods hasFlag Flag.BYNAMEPARAM) && !(tree.mods hasFlag Flag.IMPLICIT))
              c.abort(tree.pos, "Multitier module arguments must be call-by-name or implicit")

            if (!(tree.mods hasFlag Flag.LOCAL))
              c.abort(tree.pos, "Multitier module arguments cannot be class values")

            Seq(ModuleValue(tree.symbol, tree))

          case tree: ValDef =>
            logging.debug(s" Collecting value ${tree.name}")

            splitValOrDefDef(tree) { (tree, peer, tpe, tpt, modality, specialized) =>
              erase(
                tree.rhs, tree.symbol,
                peer, tpe, tpt, modality,
                specialized,
                rhs => tree map { (mods, name, tpt, _) => (mods, name, tpt, rhs) })
            }

          case tree: ModuleDef if isMultitierModule(tree.symbol.info, tree.pos) =>
            logging.debug(s" Collecting multitier module ${tree.name}")
            splitMultitierModule(tree)

          case tree: ClassDef if isMultitierModule(tree.symbol.companion.info, tree.pos) =>
            logging.debug(s" Collecting multitier module companion ${tree.name}")
            splitMultitierModuleCompanionClass(tree)

          case tree: MemberDef =>
            if (tree.symbol != NoSymbol)
              logging.debug(s" Collecting ${tree.symbol}")
            else
              logging.debug(s" Collecting ${tree.name}")

            Seq(ModuleValue(tree.symbol, tree))

          case tree: Import =>
            logging.debug(" Collecting import statement")
            Seq(ModuleValue(tree.symbol, tree))

          case tree @ q"$_[..$_](...$exprss)"
              if tree.nonEmpty &&
                 tree.symbol != null &&
                 (tree.symbol.owner == symbols.On ||
                  tree.symbol.owner == symbols.Placed) =>
            decomposePlacementType(tree.tpe, EmptyTree, NoSymbol, tree.pos, moduleDefinition = true) match {
              case Placed(peer, _, _, modality) =>
                logging.debug(s" Collecting expression placed on ${peer.name}")
                erase(
                  stripPlacementSyntax(tree), NoSymbol,
                  peer, definitions.UnitTpe, TypeTree(definitions.UnitTpe), modality,
                  List.empty,
                  identity)

              case _ =>
                logging.debug(" Collecting non-placed expression")
                Seq(PlacedValueDef(NoSymbol, tree, None, Modality.None))
            }

          case tree =>
            logging.debug(" Collecting non-placed expression")
            Seq(PlacedValueDef(NoSymbol, tree, None, Modality.None))
        }
    }) ++ concreteStubs
  }

  // erase implicit conversion that lifts standard values to placed values
  def processLiftedPlacedValues(records: List[Any]): List[Any] = {
    var count = 0
    val result = records process {
      case record @ PlacedValue(_, _, _, _) =>
        object transformer extends Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case q"$_[..$_](...$exprss)"
                if tree.nonEmpty && (symbols.lifts contains tree.symbol) =>
              count += 1
              exprss.head.head

            case _ =>
              super.transform(tree)
          }
        }

        record.copy(tree = transformer transform record.tree)
    }

    if (count == 1)
      logging.debug(s" Lifted $count non-placed value to a placed value")
    else
      logging.debug(s" Lifted $count non-placed values to placed values")

    result
  }

  // validate types of placed values
  def validatePlacedValues(records: List[Any]): List[Any] = {
    // validate placement types
    logging.debug(" Validating types of placed values")
    records foreach {
      case PlacedValueDef(symbol, tree, Some(_), modality) =>
        // ensure local placed values do not override non-local placed values
        // and placed values do not override non-placed values
        symbol.overrides foreach { overrideSymbol =>
          decomposePlacementType(overrideSymbol.info, EmptyTree, symbol, tree.pos, moduleDefinition = false) match {
            case Placed(_, _, _, overrideModality) =>
              if (modality == Modality.Local && overrideModality != Modality.Local)
                c.abort(tree.pos,
                  s"Local placed declaration ${symbol.nameInEnclosing} cannot override " +
                  s"non-local placed declaration ${overrideSymbol.nameInEnclosing}")

            case _ =>
              c.abort(tree.pos,
                s"Placed declaration ${symbol.nameInEnclosing} cannot override " +
                s"non-placed declaration ${overrideSymbol.nameInEnclosing}")
          }
        }

        // ensure placed statements are neither subjective nor local
        if (symbol == NoSymbol)
          modality match {
            case Modality.Local =>
              c.abort(tree.pos, "Placed statements cannot be local")
            case Modality.Subjective(_) =>
              c.abort(tree.pos, "Placed statements cannot be subjective")
            case _ =>
          }

      case _ =>
    }

    // validate access to placed values
    def validate(tree: Tree, peerType: Option[Type]) = {
      tree foreach {
        case tree: RefTree if tree.tpe != null =>
          val treeType =
            if (tree.symbol.isTerm && tree.symbol.asTerm.isSetter)
              tree.symbol.asTerm.getter.info.finalResultType.asSeenFrom(module.classSymbol)
            else
              tree.tpe.asSeenFrom(module.classSymbol)

          if (treeType real_<:< types.placedValue) {
            val localPeer = peerType exists { tpe =>
              val Seq(_, peer) = extractTag(treeType, types.placedValue, tree.pos).typeArgs: @unchecked
              tpe <:< peer
            }

            if (!localPeer)
              c.abort(tree.pos, accessMessage)
          }
          else if ((tree.tpe.finalResultType.baseClasses exists { _.owner == symbols.placement }) ||
                   tree.symbol == symbols.and)
            c.abort(tree.pos, "Unexpected multitier construct")

        case tree: TypeTree
            if tree.original != null &&
               internal.attachments(tree.original).contains[preprocessors.StatedTyping.type] =>
          val treeTypes = Option(tree.tpe) ++ Option(tree.original.tpe)

          val placementType = treeTypes exists {
            _.underlying exists {
              case TypeRef(_, symbols.local, _) =>
                true
              case tpe =>
                placementTypes exists { tpe.underlying real_<:< _ }
            }
          }

          if (placementType)
            c.abort(tree.pos, "Illegal usage of placement type")

        case _ =>
      }
    }

    logging.debug(" Validating accesses of placed values")

    records foreach {
      case ModuleValue(_, _: ValOrDefDef) =>
      case ModuleValue(symbol, tree) if !isMultitierModule(symbol.info, tree.pos) =>
        validate(tree, None)
      case PlacedValue(_, tree, peer, _) =>
        validate(tree, peer map { _.asType.toType.asSeenFrom(module.classSymbol) })
      case _ =>
    }

    records
  }

  // fix self and super references to the enclosing module
  def fixEnclosingReferences(records: List[Any]): List[Any] = {
    logging.debug(" Processing references to definitions of the multitier module after expansion")

    (records
      // fix references to expanding module that will be wrong
      // after moving the code to an inner trait
      process {
        case value: Value =>
          object transformer extends Transformer {
            def underExpansion(tree: Tree): Boolean = tree match {
              case tree: TypeTree if tree.tpe != null && tree.original == null =>
                def hasOwner(symbol: Symbol): Boolean =
                  symbol != NoSymbol && (symbol == module.classSymbol || hasOwner(symbol.owner))

                tree.tpe exists {
                  case ThisType(sym) => hasOwner(sym)
                  case _ => false
                }

              case _ =>
                false
            }

            override def transform(tree: Tree): Tree = tree match {
              // inferred type trees with no correspondence in the original source code
              case tree: ValOrDefDef if underExpansion(tree.tpt) =>
                tree map { (mods, name, _, rhs) =>
                  (transformModifiers(mods),
                   name,
                   if (tree.symbol.isParameter)
                     createTypeTree(tree.tpt.tpe, tree.pos)
                   else
                     TypeTree(),
                   transform(rhs))
                }

              case tree: TypeTree if underExpansion(tree) =>
                val tpe = tree.tpe map { tpe =>
                  if (tpe.typeArgs.size == 2 && (tpe real_<:< types.per))
                    types.function mapArgs { _ => List(
                      types.remote mapArgs { _ => tpe.typeArgs.tail },
                      tpe.typeArgs.head) }
                  else if ((tpe.typeArgs.size == 1 || tpe.typeArgs.size == 2) &&
                           (placementTypes exists { tpe real_<:< _ }))
                    tpe.typeArgs.head
                  else
                    tpe
                }

                createTypeTree(tpe, tree.pos)

              // type trees with correspondence in the original source code
              case tree: TypeTree if tree.original != null =>
                internal.setOriginal(TypeTree(), transform(tree.original))

              // any other tree
              case _ =>
                super.transform(tree)
            }
          }

          value.copy(tree = transformer transform value.tree)
      }
      process {
        case value: PlacedValuePeerImpl =>
          value.copy(tree = fixEnclosingReferences(value.tree, requirePeerType(value.peer).name))
        case value: PlacedValueDef =>
          value.copy(tree = fixEnclosingReferences(value.tree, names.placedValues(module.symbol)))
      })
  }

  private def fixEnclosingReferences(tree: Tree, enclosingName: TypeName) = {
    object transformer extends Transformer {
      val skippedTrees = mutable.Set.empty[Tree]

      def skip(tree: Tree): Unit = tree match {
        case Select(_, _) | Super(_, _) =>
          skippedTrees += tree
          skip(tree.children.head)
        case _ =>
          skippedTrees += tree
      }

      def fixTypeReference(tree: Tree): Tree = tree match {
        case Select(qualifier, name) =>
          treeCopy.Select(tree, fixTypeReference(qualifier), name)
        case This(_) if tree.symbol == module.classSymbol =>
          Ident(module.self)
        case _ =>
          transform(tree)
      }

      override def transform(tree: Tree): Tree = tree match {
        case tree: TypeTree if tree.original != null =>
          internal.setOriginal(TypeTree(), transform(tree.original))

        case Select(qualifier, name)
          if (qualifier.symbol == module.symbol ||
              qualifier.symbol == module.classSymbol) &&
             (name.toString contains "$default$") &&
              (tree.symbol == NoSymbol ||
               tree.symbol.isTerm && tree.symbol.isSynthetic && tree.symbol.asTerm.isParamWithDefault) =>
          skip(tree)
          super.transform(tree)

        case tree: RefTree if tree.symbol.isConstructor =>
          skip(tree)
          super.transform(tree)

        case tree: Select if tree.symbol.isType =>
          fixTypeReference(tree)

        case tree: This if !(skippedTrees contains tree) =>
          (moduleStablePath(tree.symbol, This(enclosingName))
            getOrElse super.transform(tree))

        case tree: Ident if !(skippedTrees contains tree) =>
          (moduleStablePath(tree.tpe, This(enclosingName))
            getOrElse super.transform(tree))

        case tree: RefTree if !(skippedTrees contains tree) && tree.symbol == module.symbol =>
          This(enclosingName)

        case _ =>
          super.transform(tree)
      }
    }

    transformer transform tree
  }

  // construct the stable path within a multitier module for a symbol
  // using a given prefix tree if the symbol has a stable path
  // within the module
  // module-stable implementations take part in lifting/unlifting
  // this method assumes that the instance referenced by the symbol
  // is part of this module instance
  def moduleStablePath(symbol: Symbol, prefix: Tree): Option[Tree] =
    if (module.symbol.info.baseClasses contains symbol)
      Some(prefix)
    else if (symbol.isModuleClass || symbol.isModule)
      moduleStablePath(symbol.owner, prefix) map { tree =>
        val moduleSymbol = if (symbol.isClass) symbol.asClass.module else symbol
        internal.setSymbol(Select(tree, symbol.name.toTermName), moduleSymbol)
      }
    else
      None

  // construct the stable path within a multitier module for a type
  // using a given prefix tree if the symbol has a stable path
  // within this module instance
  // module-stable implementations take part in lifting/unlifting
  def moduleStablePath(tpe: Type, prefix: Tree): Option[Tree] = tpe match {
    case SingleType(_, module.symbol) =>
      Some(prefix)
    case SingleType(pre, sym)
        if sym.isModule || sym.isModuleClass || sym.isTerm && sym.asTerm.isStable =>
      moduleStablePath(pre, prefix) map { tree =>
        internal.setSymbol(Select(tree, sym.name), sym)
      }
    case TypeRef(_, sym, List())
        if sym.isModule || sym.isModuleClass || sym.isTerm && sym.asTerm.isStable =>
      moduleStablePath(sym, prefix)
    case ThisType(sym) =>
      moduleStablePath(sym, prefix)
    case _ =>
      None
  }

  def isMultitierModule(tpe: Type, pos: Position): Boolean = {
    val finalType = tpe.finalResultType

    val isMultitierModule = finalType.baseClasses take 2 exists { symbol =>
      symbol == module.symbol ||
      symbol == module.classSymbol ||
      (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule })
    }

    if (isMultitierModule)
      finalType match {
        case RefinedType(_, decls) if decls.nonEmpty =>
          c.abort(finalType.typeSymbol.pos orElse pos,
            "Type refinements not allowed for multitier modules")
        case _ =>
      }

    isMultitierModule
  }

  def decomposePlacementType(tpe: Type, tpt: Tree, symbol: Symbol, pos: Position, moduleDefinition: Boolean): Placement =
    tpe.finalResultType.asSeenFrom(module.classSymbol) match {
      // placed value
      case TypeRef(_, sym, List(valueType, peerType))
          if sym == symbols.on ||
             sym == symbols.from ||
             sym == symbols.fromSingle ||
             sym == symbols.fromMultiple =>
        val peerSymbol = peerType.typeSymbol
        val valueTree = tpt.original match {
          case AppliedTypeTree(_, List(valueTree, _)) => valueTree
          case _ => EmptyTree
        }

        // ensure value is placed on a peer
        requirePeerType(peerSymbol, pos)

        // ensure the peer, on which the value is placed,
        // is defined in the same module
        if (moduleDefinition && !modulePeer(peerType))
          c.abort(pos, s"${if (symbol != NoSymbol) symbol else "Statement"} " +
            "cannot be placed on peer of another module")

        valueType match {
          // modality: subjective
          case TypeRef(_, symbols.per, List(subjectiveValueType, subjectivePeerType)) =>
            val subjectiveType =
              types.function mapArgs { _ => List(
                types.remote mapArgs { _ => List(subjectivePeerType) },
                subjectiveValueType)
              }

            val subjectiveTree = valueTree.original match {
              case AppliedTypeTree(_, List(subjectiveValueTree, subjectivePeerTree)) =>
                tq"${createTypeTree(types.remote.typeConstructor, NoPosition)}[$subjectivePeerTree] => $subjectiveValueTree"
              case _ => EmptyTree
            }

            requirePeerType(subjectivePeerType.typeSymbol, pos)

            validatePlacedType(subjectiveType, pos)
            Placed(peerSymbol, subjectiveType, subjectiveTree, Modality.Subjective(subjectivePeerType))

          // modality: subjective, but wrong syntax
          case tpe if tpe real_<:< types.subjective =>
            val Seq(value, peer) = extractTag(tpe, types.subjective, pos).typeArgs: @unchecked
            c.abort(pos, s"Subjective placed type must be given as: $value per ${peer.typeSymbol.name}")

          // modality: local
          case TypeRef(_, symbols.local, List(localValueType)) =>
            val localValueTree = valueTree.original match {
              case AppliedTypeTree(_, List(localValueTree)) => localValueTree
              case _ => EmptyTree
            }

            validatePlacedType(localValueType, pos)
            Placed(peerSymbol, localValueType, localValueTree, Modality.Local)

          // modality: none
          case _ =>
            validatePlacedType(valueType, pos)
            Placed(peerSymbol, valueType, valueTree, Modality.None)
        }

      // non-placed value
      case tpe =>
        // wrong syntax for placed values
        if (tpe real_<:< types.placedValue) {
          val Seq(value, peer) = extractTag(tpe, types.placedValue, pos).typeArgs: @unchecked

          val (selectedValue, selection) =
            if (value real_<:< types.singleSelection)
              extractTag(value, types.singleSelection, pos).typeArgs.head -> "fromSingle"
            else if (value real_<:< types.multipleSelection)
              extractTag(value, types.multipleSelection, pos).typeArgs.head -> "fromMultiple"
            else
              value -> "on"

          val subjectiveValue =
            if (selectedValue real_<:< types.subjective) {
              val Seq(value, peer) = extractTag(selectedValue, types.subjective, pos).typeArgs: @unchecked
              s"$value per ${peer.typeSymbol.name}"
            }
            else
              s"$selectedValue"

          c.abort(pos, s"Placed type must be given as: $subjectiveValue $selection ${peer.typeSymbol.name}")
        }

        validatePlacedType(tpe, pos)
        NonPlaced
    }

  private def validatePlacedType(tpe: Type, pos: Position) = {
    val invalidType = tpe find {
      case TypeRef(_, symbols.local, _) => true
      case tpe => (tpe real_<:< types.placedValue) || (tpe real_<:< types.subjective)
    }

    invalidType foreach { invalidType =>
      c.abort(pos, s"Invalid nesting of placement types: $invalidType")
    }
  }

  def extractTag(tpe: Type, tag: Type, pos: Position): Type = {
    val extractedTag = tpe.underlying match {
      case RefinedType(parents, _) =>
        val tags = parents filter { _ real_<:< tag }
        if (tags.size == 1)
          extractTag(tags.head, tag, pos)
        else
          NoType

      case tpe =>
        tpe
    }

    if (extractedTag real_<:!< tag)
      c.abort(pos, s"Could not find unique tag: $tag")

    extractedTag
  }

  // lifts the qualified visibility scope to the enclosing owner
  // in case the visibility is more tightly scoped
  private def liftPrivateWithin(privateWithin: Symbol, scopeRestrictionFlag: Boolean): Name = {
    def isPrivateWithinModule(symbol: Symbol): Boolean =
      symbol != NoSymbol && (symbol == module.classSymbol || isPrivateWithinModule(symbol.owner))

    val liftedPrivateWithin =
      if (isPrivateWithinModule(privateWithin)) module.classSymbol else privateWithin

    if (scopeRestrictionFlag)
      (liftedPrivateWithin orElse module.classSymbol).name
    else if (liftedPrivateWithin != NoSymbol)
      liftedPrivateWithin.name
    else
      typeNames.EMPTY
  }

  // since we potentially generate value aliases to objects in nested objects
  // we lift their visibility (as little as possible) using qualified visibility modifiers
  def liftMods(symbol: Symbol, mods: Modifiers): Modifiers = {
    val scopeRestrictionFlag =
      (mods hasFlag Flag.LOCAL) ||
      (mods hasFlag Flag.PRIVATE) ||
      (mods hasFlag Flag.PROTECTED)

    // remove `private` and `local` flags since they cannot be used in conjunction
    // with `privateWithin` (as opposed to the `protected` flag)
    Modifiers(
      (mods withoutFlags (Flag.PRIVATE | Flag.LOCAL)).flags,
      liftPrivateWithin(symbol.privateWithin, scopeRestrictionFlag),
      mods.annotations)
  }

  private val multitierInitializations: Map[Symbol, List[Symbol]] = {
    val multitierInitializations = mutable.Map.empty[Symbol, mutable.Set[Symbol]]

    def findInitializations(tree: Tree): Seq[Symbol] =
      tree match {
        case Apply(_, _) | TypeApply(_, _) | Typed(_, _) =>
          findInitializations(tree.children.head)
        case Block(_, expr) =>
          findInitializations(expr)
        case Match(_, cases) =>
          cases flatMap findInitializations
        case If(_, _, _) =>
          tree.children.tail flatMap findInitializations
        case Try(_, _, _) =>
          tree.children dropRight 1 flatMap findInitializations
        case Assign(_, _) | Function(_, _) | Literal(_) | Throw(_) | _: DefTree =>
          Seq.empty
        case Select(New(_), termNames.CONSTRUCTOR) =>
          Seq(tree.symbol.owner)
        case Select(_, TermName("asInstanceOf")) =>
          findInitializations(tree.children.head)
        case Select(_, _) =>
          Seq(tree.symbol)
        case _ if tree.children.isEmpty =>
          Seq(tree.symbol)
        case _ =>
          tree.children flatMap findInitializations
      }

    module.tree.impl foreach {
      case tree: ValOrDefDef
          if tree.symbol.isTerm &&
             !tree.symbol.isConstructor &&
             isMultitierModule(tree.tpt.tpe, tree.pos) =>
        val initializations = findInitializations(tree.rhs)
        if (initializations.nonEmpty)
          multitierInitializations.getOrElseUpdate(
            tree.symbol,
            mutable.Set.empty[Symbol]) ++= initializations

      case _ =>
    }

    // search for new initializations until reaching a fixed point
    var foundAdditionals = true
    while (foundAdditionals) {
      foundAdditionals = false
      multitierInitializations foreach { case (_, initializations) =>
        val additionals =
          (initializations.toSet flatMap multitierInitializations.get).flatten -- initializations
        if (additionals.nonEmpty) {
          initializations ++= additionals
          foundAdditionals = true
        }
      }
    }

    (multitierInitializations.iterator map { case (symbol, initializations) =>
      symbol -> (initializations.toList sortBy { symbol =>
        if (symbol.isTerm) {
          val termSymbol = symbol.asTerm
          (if (termSymbol.isStable) -1 else 0) + (if (termSymbol.isAccessor) -1 else 0) - 1
        }
        else
          0
      })
    }).toMap
  }

  private val placementTypes = List(
    types.placedValue, types.subjective, types.singleSelection, types.multipleSelection)

  private val accessMessage = "Access to abstraction " +
    "only allowed on peers on which the abstraction is placed. " +
    "Remote access must be explicit."

  private val accessAnnotation =
    internal.setType(q"new ${types.compileTimeOnly}($accessMessage)", types.compileTimeOnly)

  private val multitierStubAnnotation =
    internal.setType(q"new ${types.multitierStub}", types.multitierStub)

  private val abstractValueAnnotation =
    internal.setType(q"new ${types.abstractValue}", types.abstractValue)

  private def changeType(tree: ValOrDefDef, tpe: Type, tpt: Tree) =
    tree map { (mods, name, _, rhs) =>
      (mods, name, tpt orElse TypeTree(tpe), rhs)
    }

  private def extractValue(name: TermName, tpe: Type, tpt: Tree, rhs: Tree, pos: Position) =
    atPos(pos) { q"${Flag.SYNTHETIC} protected[this] def $name(): ${createTypeTree(tpt orElse TypeTree(tpe))} = $rhs" }

  private def eraseValue(name: TermName, tpe: Type, tpt: Tree, pos: Position) = {
    val tree = createTypeTree(tpt orElse TypeTree(tpe))
    atPos(pos) { q"${Flag.SYNTHETIC} protected[this] def $name(): $tree = null.asInstanceOf[$tree]" }
  }

  private def eraseValue(tree: ValOrDefDef) =
    tree map { (mods, name, _, rhs) =>
      val tpt = createTypeTree(tree.tpt)
      val annotations =
        if (!tree.symbol.isPrivateThis)
          List(accessAnnotation, multitierStubAnnotation)
        else
          List.empty
      (mods mapAnnotations { annotations ++ _ },
        name, tpt,
        if (tree.symbol.isAbstract) rhs else q"null.asInstanceOf[$tpt]")
    }

  private def splitPlacementSyntax(tree: Tree): List[Tree] = tree match {
    case q"$expr.$_[..$_](...$exprss)"
        if tree.nonEmpty && tree.symbol == symbols.and =>
      exprss.head.head :: splitPlacementSyntax(expr)

    case _ =>
      tree :: Nil
  }

  private def stripPlacementSyntax(tree: ValOrDefDef): ValOrDefDef =
    tree map { (mods, name, tpt, rhs) =>
      (mods, name, tpt, stripPlacementSyntax(rhs))
    }

  private def stripPlacementSyntax(tree: Tree): Tree = tree match {
    case q"$_[..$_](...$exprss)"
        if tree.nonEmpty &&
           (tree.symbol.owner == symbols.On ||
            tree.symbol.owner == symbols.Placed) =>
      val q"(..$_) => $expr" = exprss.head.head: @unchecked
      expr

    case _ =>
      tree
  }
}
