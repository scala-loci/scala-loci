package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Impls extends Component.Factory[Impls](
    requires = Seq(Commons, ModuleInfo, Peers, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Impls(engine)
  def asInstance[C <: blackbox.Context] = { case c: Impls[C] => c }
}

class Impls[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("impls:lift", liftNestedImplementations, after = Set("values:collect"), before = Set("values:fixrefs")))

  val commons = engine.require(Commons)
  val moduleInfo = engine.require(ModuleInfo)
  val peers = engine.require(Peers)
  val values = engine.require(Values)

  import engine._
  import engine.c.universe._
  import commons._
  import moduleInfo._
  import peers._
  import values._


  def liftNestedImplementations(records: List[Any]): List[Any] =
    (records
      flatProcess {
        case ModuleValue(symbol, tree: ImplDef) =>
          val (moduleValues, placedValues) = lift(tree)
          (moduleValues map { ModuleValue(symbol, _) }) ++
          (placedValues map { PlacedValueDef(symbol, _, None, Modality.None) })
      }
      process {
        case value: PlacedValuePeerImpl =>
          value.copy(tree = injectPlacedValueReferences(value.tree,
            q"${requirePeerType(value.peer).name}.this: ${module.self}.${names.placedValues}"))
        case value: Value =>
          value.copy(tree = injectPlacedValueReferences(value.tree,
            q"${names.placedValues}.this"))
      }
      process {
        case value: PlacedValue =>
          value.copy(tree = typeTreeLifter transform value.tree)
      })

  private def lift(tree: Tree): (Option[Tree], Option[Tree]) =
    lift(tree, q"${module.self}")

  private def lift(tree: Tree, valuePrefix: Tree): (Option[Tree], Option[Tree]) =
    tree match {
      case tree: DefDef if tree.symbol.isConstructor =>
        Some(tree) -> Some(tree)
      case tree: ValOrDefDef =>
        Some(eraseValue(tree)) -> Some(tree)
      case tree: ImplDef if isMultitierModule(tree.symbol.info, tree.pos) =>
        Some(tree) -> None
      case tree: ModuleDef =>
        liftModule(tree, valuePrefix)
      case tree: ClassDef =>
        liftClass(tree, valuePrefix)
      case tree: TypeDef =>
        Some(tree) -> None
      case _ =>
        None -> Some(tree)
    }

  private def liftClass(tree: ClassDef, valuePrefix: Tree): (Option[Tree], Option[Tree]) =
    if (isUnlifted(tree.symbol)) {
      val ClassDef(_, name, tparams, Template(parents, self, _)) = tree

      val mods = tree.mods mapAnnotations { multitierStubAnnotation :: _ }

      val placedValues = names.placedValues.toTermName

      val classDef =
        if (tree.mods hasFlag Flag.TRAIT) {
          val body =
            q"$placedValues" ::
            q"${Flag.SYNTHETIC} protected[this] def $placedValues: ${names.placedValues}" ::
            tree.impl.body

          treeCopy.ClassDef(tree, mods, name, tparams, Template(parents, self, body))
        }
        else {
          def implicitArgument(flags: FlagSet) = ValDef(
            Modifiers(Flag.PARAMACCESSOR | Flag.SYNTHETIC | flags),
            placedValues,
            Ident(names.placedValues),
            EmptyTree)

          val body =
            implicitArgument(Flag.PROTECTED | Flag.LOCAL) ::
            (tree.impl.body process {
              case tree @ DefDef(mods, name, tparams, _, tpt, rhs) if tree.symbol.isConstructor =>
                val vparamss =
                  if (takesImplicits(tree.symbol.asMethod))
                    (tree.vparamss dropRight 1) :+ (tree.vparamss.last :+ implicitArgument(Flag.IMPLICIT | Flag.PARAM))
                  else
                    tree.vparamss :+ List(implicitArgument(Flag.IMPLICIT | Flag.PARAM))

                treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
            })

          treeCopy.ClassDef(tree, mods, name, tparams, Template(parents, self, body))
        }

      Some(termTreeUnlifter transform classDef) -> None
    }
    else
      Some(tree) -> None

  private def liftModule(tree: ModuleDef, valuePrefix: Tree): (Option[Tree], Option[Tree]) =
    if (isUnlifted(tree.symbol)) {
      val ModuleDef(mods, name, Template(parents, self, body)) = tree

      val companion = tree.symbol.companion

      if (companion.isClass && companion.asClass.isCaseClass)
        c.abort(tree.pos, "Case class companion objects may not refer to definitions of the multitier module")

      if (tree.mods hasFlag Flag.CASE)
        c.abort(tree.pos, "Case objects may not refer to definitions of the multitier module")

      val (moduleValues, placedValues) = (body map { lift(_, q"$valuePrefix.$name") }).unzip

      Some(treeCopy.ModuleDef(tree, mods, name, Template(parents, self, moduleValues.flatten))) ->
        Some(treeCopy.ModuleDef(tree, liftMods(tree.symbol, mods), name, Template(parents, self, placedValues.flatten)))
    }
    else {
      val mods = if (tree.symbol.owner == module.classSymbol) tree.mods else liftMods(tree.symbol, tree.mods)

      Some(treeCopy.ModuleDef(tree, mods, tree.name, tree.impl)) ->
        Some(q"${moduleAliasMods(tree.mods)} val ${tree.name} = $valuePrefix.${tree.name}")
    }

  // the visibility for value aliases to objects is the same as defined for the object
  private def moduleAliasMods(mods: Modifiers) = Modifiers(
    Flag.FINAL | Flag.LAZY |
      (if (mods hasFlag Flag.LOCAL) Flag.LOCAL else NoFlags) |
      (if (mods hasFlag Flag.PRIVATE) Flag.PRIVATE else NoFlags) |
      (if (mods hasFlag Flag.PROTECTED) Flag.PROTECTED else NoFlags),
    mods.privateWithin)

  private def takesImplicits(symbol: MethodSymbol): Boolean = {
    val paramLists = symbol.asMethod.paramLists
    paramLists.nonEmpty && paramLists.last.nonEmpty && paramLists.last.head.isImplicit
  }

  private def addImplicitArgument(tree: Tree, arg: Tree): Tree =
      if (takesImplicits(tree.symbol.asMethod)) {
        val Apply(fun, args) = tree
        treeCopy.Apply(tree, fun, args :+ arg)
      }
      else
        treeCopy.Apply(tree, tree, List(arg))

  private def enclosingPlacedValueReference(symbol: Symbol, enclosing: Tree, pos: Position): Tree = {
    def enclosingUnliftedClass(symbol: Symbol): Boolean =
      symbol != NoSymbol &&
      symbol != module.classSymbol &&
      ((symbol.isClass && !symbol.isModuleClass && isUnlifted(symbol)) ||
        enclosingUnliftedClass(symbol.owner))

    atPos(pos) {
      if (enclosingUnliftedClass(symbol))
        q"${names.placedValues.toTermName}"
      else
        enclosing
    }
  }

  // construct the stable prefix path within a multitier module
  // consisting of multitier module instances
  private def multitierPrefixPath(tpe: Type): Option[String] = {
    def multitierPrefixPath(tree: Tree, multitier: Boolean): Option[String] = tree match {
      case EmptyTree =>
        Some("")
      case Select(qualifier, name) =>
        if (isMultitierModule(tree.symbol.info, tree.pos))
          multitierPrefixPath(qualifier, multitier = true) map { path => s"$path.$name" }
        else if (!multitier)
          multitierPrefixPath(qualifier, multitier = false)
        else
          None
    }

    moduleStablePath(tpe, EmptyTree) flatMap {
      multitierPrefixPath(_, multitier = false) map { _ drop 1 }
    }
  }

  // does the symbol have a stable path within a multitier module
  // module-stable implementations take part in lifting/unlifting
  private def isModuleStable(symbol: Symbol): Boolean =
    moduleStablePath(symbol, EmptyTree).nonEmpty

  private def isUnlifted(symbol: Symbol) =
    (unliftables contains symbol) && isModuleStable(symbol.owner)

  private def isLocalUnlifted(symbol: Symbol) =
    (unliftables contains symbol) && !isModuleStable(symbol.owner)

  private def isDummyPlacedValueReference(tree: DefDef) =
    (tree.mods hasFlag Flag.SYNTHETIC) &&
    tree.name == names.placedValues.toTermName &&
    tree.symbol.info.finalResultType =:= definitions.NothingTpe

  // unlift value references which are stable within the module
  // to references to the synthetic placed value reference of the enclosing class
  private object termTreeUnlifter extends Transformer {
    val placedValues = Ident(names.placedValues.toTermName)

    override def transform(tree: Tree): Tree = tree match {
      case tree: TypeTree if tree.original != null =>
        internal.setOriginal(TypeTree(), transform(tree.original))
      case tree: This =>
        (moduleStablePath(tree.symbol, placedValues)
          getOrElse super.transform(tree))
      case tree: Ident if tree.isTerm =>
        (moduleStablePath(tree.symbol.owner, placedValues)
          map { treeCopy.Select(tree, _, tree.name) }
          getOrElse super.transform(tree))
      case tree: SingletonTypeTree if tree.tpe.typeSymbol.isModuleClass =>
        (moduleStablePath(tree.tpe.typeSymbol, placedValues)
          map { treeCopy.SingletonTypeTree(tree, _) }
          getOrElse super.transform(tree))
      case tree: Select if tree.isType && !tree.symbol.isModuleClass =>
        tree
      case _ =>
        super.transform(tree)
    }
  }

  // lift type references to types which are stable within the module
  private object typeTreeLifter extends Transformer {
    def multitierMember(tree: Tree): Boolean = tree match {
      case Select(Ident(_), _) => isMultitierModule(tree.symbol.info, tree.pos)
      case Select(qualifier, _) => multitierMember(qualifier)
      case _ => true
    }

    def moduleMember(tree: Tree): Boolean = tree match {
      case Select(qualifier, _) =>
        (tree.symbol.isModule || tree.symbol.isModuleClass) && moduleMember(qualifier)
      case _ =>
        true
    }

    override def transform(tree: Tree): Tree = tree match {
      case tree: TypeTree if tree.original != null =>
        internal.setOriginal(TypeTree(), transform(tree.original))
      case Select(qualifier, name) if tree.isType =>
        (moduleStablePath(qualifier.tpe, q"${module.self}")
          filter { tree => multitierMember(tree) || moduleMember(tree) }
          map { treeCopy.Select(tree, _, name) }
          getOrElse super.transform(tree))
      case _ =>
        super.transform(tree)
    }
  }

  // create synthetic placed value references
  // potentially replacing or removing the dummy references created implicitly
  // or inserted by the preprocessor
  private def injectPlacedValueReferences(tree: Tree, enclosing: Tree): Tree = {
    val skippedTrees = mutable.Set.empty[Tree]

    object placedValueReferencesCreator extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        // parents and synthetic placed value references for classes and traits
        case ClassDef(mods, name, tparams, impl) =>
          val classSymbol = tree.symbol

          // since we potentially need to lift module-stable parent types
          // they are required to have a tree representation
          val parents = impl.parents map {
            case tree: TypeTree
                if tree.tpe != null &&
                   tree.original == null &&
                   isModuleStable(tree.symbol.owner) =>
              createTypeTree(tree.tpe, tree.pos)
            case tree: TypeTree
                if tree.tpe == null &&
                  tree.original != null =>
              tree.original
            case tree =>
              tree
          }

          parents foreach validateInstantiatability

          val body = impl.body flatMap {
            // manually insert the synthetic placed value reference
            // into the implicit argument list of super or primary constructor calls
            // to the constructor of classes which are currently under expansion
            // and are just being extended with the additional implicit argument
            // thus, type inference did not resolve the argument yet
            case tree @ DefDef(mods, name, tparams, vparamss, tpt, block @ Block(_, expr))
                if tree.symbol.isConstructor =>
              val stats = block.stats process {
                case tree: Apply
                    if tree.symbol.isConstructor &&
                       isUnlifted(tree.symbol.owner) &&
                       underExpansion(tree.symbol.owner) =>
                  if (isModuleStable(classSymbol.owner))
                    addImplicitArgument(tree, q"${names.placedValues.toTermName}")
                  else
                    addImplicitArgument(tree, enclosingPlacedValueReference(classSymbol.owner, enclosing, classSymbol.pos))
              }
              Some(treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt,
                treeCopy.Block(block, stats, expr)))

            // replace or remove dummy synthetic placed value references
            // that were inserted by the preprocessor
            case tree: DefDef if isDummyPlacedValueReference(tree) =>
              val parentClass = parents.head.tpe.dealias.typeSymbol

              val inheritsImplicitPlacedValueReference =
                parentClass.isClass && !parentClass.asClass.isTrait && isUnlifted(parentClass)

              val hasModuleStableParents =
                classSymbol.info.baseClasses exists { symbol => isModuleStable(symbol.owner) }

              if (isLocalUnlifted(classSymbol) &&
                  hasModuleStableParents &&
                  !inheritsImplicitPlacedValueReference)
                Some(q"""${Flag.SYNTHETIC} protected[this] def ${names.placedValues.toTermName}: ${names.placedValues} =
                           ${enclosingPlacedValueReference(classSymbol.owner, enclosing, classSymbol.pos)}""")
              else
                None

            case tree =>
              Some(tree)
          }

          super.transform(
            treeCopy.ClassDef(tree, mods, name, tparams,
              treeCopy.Template(impl, parents, impl.self, body)))

        // implicitly resolved dummy for synthetic placed value reference
        case q"$expr.$_[..$_]" if expr.symbol == symbols.placedValues =>
          enclosingPlacedValueReference(currentOwner, enclosing, tree.pos)

        // class instantiation
        case q"new $expr[..$tpts](...$exprss)" if expr.tpe != null =>
          validateInstantiatability(expr)

          if (isUnlifted(expr.symbol) &&
              underExpansion(expr.symbol) &&
              !(skippedTrees contains expr)) {
            skippedTrees += expr
            addImplicitArgument(
              super.transform(tree),
              enclosingPlacedValueReference(currentOwner, enclosing, tree.pos))
          }
          else
            super.transform(tree)

        case _ =>
          super.transform(tree)
      }
    }

    placedValueReferencesCreator transform tree
  }

  // we cannot instantiate unlifted multitier classes and traits
  // of other multitier module instances
  private def validateInstantiatability(tree: Tree): Unit =
    if (tree.tpe != null) {
      val TypeRef(pre, _, _) = tree.tpe

      multitierPrefixPath(pre) foreach { path =>
        if (path.nonEmpty && isUnlifted(tree.symbol))
          c.abort(tree.pos,
            s"Multitier type can only be instantiated in module in which it is defined: $path")
      }
    }

  private val unliftables: Set[Symbol] = {
    val unliftables = mutable.Set.empty[Symbol]
    val dependencies = mutable.Map.empty[Symbol, mutable.ListBuffer[Symbol]]

    // initially, dependent values are all module-level vals and defs and
    // all vals nested inside other objects (no vals or defs inside classes or traits)
    object traverser extends Traverser {
      var moduleStackSize = 0
      override def traverse(tree: Tree) = tree match {
        case _: ImplDef if isMultitierModule(tree.symbol.info, tree.pos) =>
          val currentModuleStackSize = moduleStackSize
          moduleStackSize = 0
          super.traverse(tree)
          moduleStackSize = currentModuleStackSize
        case _: ClassDef =>
        case _: ModuleDef =>
          moduleStackSize += 1
          super.traverse(tree)
          moduleStackSize -= 1
        case tree: ValDef
              if tree.symbol != NoSymbol &&
                 !isMultitierModule(tree.tpt.tpe, tree.pos) &&
                 !tree.symbol.isParameter =>
            unliftables += tree.symbol
          super.traverse(tree)
        case tree: DefDef
              if tree.symbol != NoSymbol &&
                 !isMultitierModule(tree.tpt.tpe, tree.pos) &&
                 !tree.symbol.isConstructor &&
                 moduleStackSize == 0 =>
            unliftables += tree.symbol
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }

    traverser traverseTrees module.tree.impl.body

    // compute dependency list for every definition
    module.tree foreach {
      case tree: DefTree
          if tree.symbol != NoSymbol &&
             tree.symbol != module.symbol &&
             tree.symbol != module.classSymbol &&
             !tree.symbol.isConstructor =>

        val list = dependencies.getOrElseUpdate(
          tree.symbol,
          mutable.ListBuffer.empty[Symbol])

        object traverser extends Traverser {
          def addDependency(symbol: Symbol): Unit =
            // if the definition references a stub value,
            // both the stub value and the definition are unliftable
            if (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierStub }) {
              unliftables += symbol
              unliftables += tree.symbol
              dependencies -= tree.symbol
            }
            // we do not add module-stable symbols to the dependency list
            // since they are split into a lifted and an unlifted part
            else if (!isModuleStable(symbol))
              list += symbol

          override def traverse(tree: Tree): Unit = tree match {
            case tree: DefDef if tree.mods hasFlag Flag.CASE | Flag.SYNTHETIC =>
            case _ =>
              // add the symbol as well as the symbol of the dealiased type to the dependency list
              if (tree.symbol != null && tree.symbol != NoSymbol) {
                addDependency(tree.symbol)
                if (tree.symbol.isType && tree.symbol.asType.isAliasType)
                  addDependency(tree.symbol.info.dealias.typeSymbol)
              }
              super.traverse(tree)
          }
        }

        traverser traverse tree

      case _ =>
    }

    // search for new unliftables inside dependency list until reaching a fixed point
    var foundAdditionals = true
    while (foundAdditionals) {
      val additionals = (dependencies collect {
        case (symbol, dependencies) if dependencies exists { unliftables contains _ } =>
          symbol
      }).toSet -- unliftables

      if (additionals.isEmpty)
        foundAdditionals = false
      else
        unliftables ++= additionals
    }

    unliftables.toSet
  }

  private val accessAnnotation = {
    val message = "Access to object member of multitier module not allowed."
    internal.setType(q"new ${types.compileTimeOnly}($message)", types.compileTimeOnly)
  }

  private val multitierStubAnnotation =
    internal.setType(q"new ${types.multitierStub}", types.multitierStub)

  private def eraseValue(tree: ValOrDefDef) =
    tree map { (mods, name, _, rhs) =>
      val tpt = createTypeTree(tree.tpt)
      (mods mapAnnotations { accessAnnotation :: multitierStubAnnotation :: _ },
        name, tpt,
        if (tree.symbol.isAbstract) rhs else q"null.asInstanceOf[$tpt]")
    }
}
