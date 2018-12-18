package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Values extends Component.Factory[Values](
    requires = Seq(ModuleInfo, Commons, Peers)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Values(engine)
  def asInstance[C <: blackbox.Context] = { case c: Values[C] => c }
}

class Values[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("values:collect", collectPlacedValues, before = Set("*", "values:validate")),
    Phase("values:validate", validatePlacedValues, before = Set("*")),
    Phase("values:fixrefs", fixEnclosingReferences, after = Set("*", "values:validate")))

  val moduleInfo = engine.require(ModuleInfo)
  val commons = engine.require(Commons)
  val peers = engine.require(Peers)

  import engine._
  import engine.c.universe._
  import moduleInfo._
  import commons._
  import peers._


  sealed trait Modality

  object Modality {
    case object None extends Modality
    case object Local extends Modality
    case class Subjective(peer: Type) extends Modality
  }

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

    // create module-level members and corresponding peer-level members
    def concretize(
        tree: ValOrDefDef)(
        concreteValues: (ValOrDefDef, Symbol, Type, Tree, Modality) => Seq[PlacedValue]): Seq[Value] =
      // for non-multitier references, create
      // 1) dummy member at module-level, which is compile-time-only,
      // 2) member at peer-level and
      // 3) (potentially) a peer-specific initializing member at the peer-level,
      //    in which case the member (2) is always non-abstract
      if (!isMultitierModule(tree.tpt.tpe)) {
        val values = destructPlacementType(tree.symbol.info, tree.tpt, tree.symbol, tree.pos) match {
          case Some((peer, tpe, tpt, modality)) =>
            if (isMultitierModule(tpe))
              c.abort(tree.pos, "Multitier module instances cannot be placed")

            val valOrDefDef = (changeType(stripPlacementSyntax(tree), tpe, tpt)
              map { (mods, name, tpt, rhs) =>
                // add `override` modifier to synthesized value if necessary
                // since we initialize synthesized values with `null`
                // instead of keeping them abstract
                if (tree.symbol.overrides.isEmpty)
                  (mods, name, tpt, rhs)
                else
                  (mods withFlags Flag.OVERRIDE, name, tpt, rhs)
              })

            if (tree.symbol.isAbstract) {
              // initialize synthesized values with `null`
              // instead of keeping them abstract
              val erasure = valOrDefDef map { (mods, name, tpt, _) =>
                (mods withoutFlags Flag.DEFERRED, name, tpt, q"null.asInstanceOf[$tpt]")
              }
              Seq(PlacedValueDef(tree.symbol, erasure, Some(peer), modality))
            }
            else if (!tree.symbol.isAbstract &&
                     (valOrDefDef.mods hasFlag Flag.MUTABLE) &&
                     valOrDefDef.rhs.isEmpty) {
              Seq(PlacedValueDef(tree.symbol, valOrDefDef, Some(peer), Modality.None))
            }
            else
              concreteValues(valOrDefDef, peer, tpe, tpt, modality)

          case _ =>
            Seq(PlacedValueDef(tree.symbol, tree, None, Modality.None))
        }

        values :+ ModuleValue(tree.symbol, eraseValue(tree))
      }
      // for multitier references,
      // 1) keep the member at module-level,
      // 2) create a reference to the multitier module's placed values at the peer-level and
      // 3) (potentially) create an initializing member at the peer-level
      else {
        val multitierName = TermName(s"$$loci$$multitier$$${tree.name}")
        val multitierType = tq"${names.multitierModule}.${tree.name}.${names.placedValues}"

        if (tree.symbol.isAbstract) {
          val application = tree map { (mods, name, _, rhs) =>
            (mods, name, multitierType, rhs)
          }

          Seq(
            PlacedValueDef(tree.symbol, application, None, Modality.None),
            ModuleValue(tree.symbol, tree))
        }
        else {
          val application = tree map { (mods, name, _, _) =>
            (mods, name, multitierType, q"$multitierName()")
          }

          val value = atPos(tree.pos) { q"protected[this] def $multitierName(): $multitierType" }

          Seq(
            PlacedValueDef(tree.symbol, application, None, Modality.None),
            PlacedValueDef(tree.symbol, value, None, Modality.None),
            ModuleValue(tree.symbol, tree))
        }
      }

    // 1) create a no-op erased dummy value,
    // 2) create an overriding peer-specific "real" value and
    // 3) create an application of the value,
    //    which might be executing a statement or initializing a member
    def erase(
        tree: Tree, symbol: Symbol, peer: Symbol, tpe: Type, tpt: Tree, modality: Modality,
        apply: Tree => Tree): Seq[PlacedValue] = {
      val exprName = TermName(s"$expr$index")
      index += 1

      val erasure = eraseValue(exprName, tpe, tpt, tree.pos)
      val value = extractValue(exprName, tpe, tpt, tree, tree.pos)
      val application = apply(q"$exprName()")

      Seq(
        PlacedValueDef(symbol, erasure, Some(peer), modality),
        PlacedValueDef(symbol, application, Some(peer), modality),
        PlacedValuePeerImpl(symbol, value, peer, modality))
    }

    // create module-level and peer-level values (including general placed values and peer-specific ones)
    // for all statements in the multitier module
    val values = module.stats flatMap {
      case tree: DefDef if tree.symbol.isTerm && !tree.symbol.isConstructor =>
        concretize(tree) { (tree, peer, _, _, modality) =>
          Seq(PlacedValueDef(tree.symbol, tree, Some(peer), modality))
        }

      case tree: ValDef if tree.symbol.asTerm.isParamAccessor =>
        if (!(tree.mods hasFlag Flag.BYNAMEPARAM))
          c.abort(tree.pos, "Multitier module arguments must be call-by-name")

        Seq(ModuleValue(tree.symbol, tree))

      case tree: ValDef =>
        concretize(tree) { (tree, peer, tpe, tpt, modality) =>
          erase(
            tree.rhs, tree.symbol,
            peer, tpe, tpt, modality,
            rhs => tree map { (mods, name, tpt, _) => (mods, name, tpt, rhs) })
        }

      case tree: MemberDef =>
        Seq(ModuleValue(tree.symbol, tree))

      case tree =>
        destructPlacementType(tree.tpe, EmptyTree, NoSymbol, tree.pos) match {
          case Some((peer, _, _, modality)) =>
            erase(
              stripPlacementSyntax(tree), NoSymbol,
              peer, definitions.UnitTpe, TypeTree(definitions.UnitTpe), modality,
              identity)

          case _ =>
            Seq(PlacedValueDef(NoSymbol, tree, None, Modality.None))
        }
    }

    // create a reference to the placed values of other multitier modules
    // which are accessed through a stable path (i.e., not part of the expanding module)
    val stableMultitierModuleValues = (module.stats flatMap {
      _ collect {
        case Select(qualifier, _) =>
          stableMultitierModuleIdentifier(qualifier).toSeq flatMap { case (delegateName, multitierName, multitierType) =>
            val delegateDefined = module.classSymbol.baseClasses.tail exists { base =>
              ((base.info member names.placedValues).info member delegateName) != NoSymbol
            }

            if (!delegateDefined) {
              val application = atPos(qualifier.pos) {
                q"final protected[this] val $delegateName: $multitierType = $multitierName()"
              }
              val value = atPos(qualifier.pos) {
                q"protected[this] def $multitierName(): $multitierType"
              }

              Seq(
                PlacedValueDef(NoSymbol, application, None, Modality.None),
                PlacedValueDef(NoSymbol, value, None, Modality.None))
            }
            else
              Seq.empty
          }
      }
    }).flatten

    records ++ stableMultitierModuleValues ++ values
  }

  // validate types of placed values
  def validatePlacedValues(records: List[Any]): List[Any] = {
    val placedSymbols = (records collect {
      case PlacedValue(symbol, _, Some(_), _) if symbol != NoSymbol => symbol
    }).toSet

    records foreach {
      case PlacedValue(symbol, tree, Some(_), modality) =>
        // ensure local placed values do not override non-local placed values
        // and placed values do not override non-placed values
        symbol.overrides foreach { overrideSymbol =>
          destructPlacementType(overrideSymbol.info, EmptyTree, symbol, tree.pos) match {
            case Some((_, _, _, overrideModality)) =>
              if (modality == Modality.Local && overrideModality != Modality.Local)
                c.abort(tree.pos,
                  s"Local placed declaration ${symbol.fullNestedName} cannot override " +
                  s"non-local placed declaration ${overrideSymbol.fullNestedName}")

            case _ =>
              c.abort(tree.pos,
                s"Placed declaration ${symbol.fullNestedName} cannot override " +
                s"non-placed declaration ${overrideSymbol.fullNestedName}")
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

      case ModuleValue(_, _: ValOrDefDef) =>

      case value: ModuleValue =>
        value.tree foreach { tree =>
          if (placedSymbols contains tree.symbol)
            c.abort(tree.pos, accessMessage)
        }

      case _ =>
    }

    records
  }

  // fix self and super references to the enclosing module
  def fixEnclosingReferences(records: List[Any]): List[Any] =
    (records
      // fix references to expanding module that will be wrong
      // after moving the code to an inner trait
      process {
        case value: Value =>
          object transformer extends Transformer {
            override def transform(tree: Tree): Tree = tree match {
              // inferred type trees with no correspondence in the original source code
              case tree: TypeTree if tree.tpe != null && tree.original == null =>
                def hasOwner(symbol: Symbol): Boolean =
                  symbol != NoSymbol && (symbol == module.classSymbol || hasOwner(symbol.owner))

                val reInferType = tree.tpe exists {
                  case ThisType(sym) => hasOwner(sym)
                  case _ => false
                }

                if (reInferType)
                  TypeTree()
                else
                  tree

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
          value.copy(tree = fixEnclosingReferences(value.tree, names.placedValues))
      })

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

      override def transform(tree: Tree): Tree = tree match {
        // type trees with correspondence in the original source code
        case tree: TypeTree if tree.original != null =>
          internal.setOriginal(TypeTree(), transform(tree.original))

        // reference to constructor call (should not be transformed)
        case tree: RefTree if tree.symbol.isConstructor =>
          skip(tree)
          super.transform(tree)

        // reference to nested this reference
        case tree: This if !(skippedTrees contains tree) =>
          (moduleStablePath(tree.symbol, This(enclosingName))
            getOrElse super.transform(tree))

        // reference to nested identifier
        case tree: Ident if !(skippedTrees contains tree) =>
          (moduleStablePath(tree.tpe, This(enclosingName))
            getOrElse super.transform(tree))

        // reference to nested selection
        case tree: RefTree if !(skippedTrees contains tree) && tree.symbol == module.symbol =>
          This(enclosingName)

        case tree: RefTree if !(skippedTrees contains tree) =>
          (stableMultitierModuleIdentifier(tree)
            map { case (delegateName, _, _) =>  q"$enclosingName.this.$delegateName" }
            getOrElse super.transform(tree))

        // any other tree
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
    case TypeRef(pre, sym, List())
        if sym.isModule || sym.isModuleClass || sym.isTerm && sym.asTerm.isStable =>
      moduleStablePath(sym, prefix)
    case ThisType(sym) =>
      moduleStablePath(sym, prefix)
    case _ =>
      None
  }

  def isMultitierModule(tpe: Type): Boolean = {
    val symbol = tpe.finalResultType.typeSymbol
    symbol == module.symbol ||
      symbol == module.classSymbol ||
      (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule })
  }

  private def stableMultitierModuleIdentifier(tree: Tree) = {
    def stablePath(symbol: Symbol): Option[Tree] =
      if (module.symbol.info.baseClasses contains symbol)
        None
      else if (symbol.isModule || symbol.isModuleClass ||
               symbol.isPackage || symbol.isPackageClass ||
               symbol.isTerm && symbol.asTerm.isStable) {
        if (symbol.owner == NoSymbol)
          Some(Ident(termNames.ROOTPKG))
        else
          stablePath(symbol.owner) map { tree =>
            val moduleSymbol = if (symbol.isModuleClass) symbol.asClass.module else symbol
            internal.setSymbol(Select(tree, symbol.name.toTermName), moduleSymbol)
          }
      }
      else
        c.abort(tree.pos,
          s"Stable identifier required to access members of multitier module $tree")

    if (tree.tpe != null && tree.symbol != module.symbol && isMultitierModule(tree.tpe))
      stablePath(tree.symbol) map { tree =>
        val name = tree.toString.substring(6).replace('.', '$')
        (TermName(s"$$loci$$delegate$$root$$$name"),
         TermName(s"$$loci$$multitier$$root$$$name"),
         tq"$tree.${names.placedValues}")
      }
    else
      None
  }

  private def destructPlacementType(tpe: Type, tpt: Tree, symbol: Symbol, pos: Position): Option[(Symbol, Type, Tree, Modality)] =
    tpe.finalResultType match {
      // placed value
      case TypeRef(_, symbols.on, List(valueType, peerType)) =>
        val peerSymbol = peerType.typeSymbol
        val valueTree = tpt.original match {
          case AppliedTypeTree(_, List(valueTree, _)) => valueTree
          case _ => EmptyTree
        }

        // ensure value is placed on a peer
        requirePeerType(peerSymbol, pos)

        // ensure the peer, on which the value is placed,
        // is defined in the same module
        if (!modulePeer(peerType))
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
                tq"${trees.remote}[$subjectivePeerTree] => $subjectiveValueTree"
              case _ => EmptyTree
            }

            validatePlacedType(subjectiveType, pos)
            Some((peerSymbol, subjectiveType, subjectiveTree, Modality.Subjective(subjectivePeerType)))

          // modality: subjective, but wrong syntax
          case tpe if tpe real_<:< types.subjective =>
            val Seq(value, peer) = extractTag(tpe, types.subjective, pos).typeArgs
            c.abort(pos, "Subjective placed type must be given as: " +
              s"${value.typeSymbol.name} per ${peer.typeSymbol.name}")

          // modality: local
          case TypeRef(_, symbols.local, List(localValueType)) =>
            val localValueTree = valueTree.original match {
              case AppliedTypeTree(_, List(localValueTree)) => localValueTree
              case _ => EmptyTree
            }

            validatePlacedType(localValueType, pos)
            Some((peerSymbol, localValueType, localValueTree, Modality.Local))

          // modality: none
          case _ =>
            validatePlacedType(valueType, pos)
            Some((peerSymbol, valueType, valueTree, Modality.None))
        }

      // non-placed value
      case tpe =>
        // wrong syntax for placed values
        if (tpe real_<:< types.placedValue) {
          val Seq(value, peer) = extractTag(tpe, types.placedValue, pos).typeArgs
          c.abort(pos, "Placed type must be given as: " +
            s"${value.typeSymbol.name} on ${peer.typeSymbol.name}")
        }

        validatePlacedType(tpe, pos)
        None
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

  private def extractTag(tpe: Type, tag: Type, pos: Position): Type = {
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

  private val accessMessage = "Access to abstraction " +
    "only allowed on peers on which the abstraction is placed. " +
    "Remote access must be explicit."

  private val accessAnnotation = q"new ${trees.compileTimeOnly}($accessMessage)"

  private val multitierStubAnnotation = q"new ${trees.multitierStub}"

  private def changeType(tree: ValOrDefDef, tpe: Type, tpt: Tree) =
    tree map { (mods, name, _, rhs) =>
      (mods, name, tpt orElse TypeTree(tpe), rhs)
    }

  private def extractValue(name: TermName, tpe: Type, tpt: Tree, rhs: Tree, pos: Position) =
    atPos(pos) { q"protected[this] override def $name(): ${createTypeTree(tpt orElse TypeTree(tpe))} = $rhs" }

  private def eraseValue(name: TermName, tpe: Type, tpt: Tree, pos: Position) = {
    val tree = createTypeTree(tpt orElse TypeTree(tpe))
    atPos(pos) { q"protected[this] def $name(): $tree = null.asInstanceOf[$tree]" }
  }

  private def eraseValue(tree: ValOrDefDef) =
    tree map { (mods, name, _, rhs) =>
      val tpt = createTypeTree(tree.tpt)
      (mods mapAnnotations { accessAnnotation :: multitierStubAnnotation :: _ },
        name, tpt,
        if (tree.symbol.isAbstract) rhs else q"null.asInstanceOf[$tpt]")
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
      val q"(..$_) => $expr" = exprss.head.head
      expr

    case _ =>
      tree
  }
}
