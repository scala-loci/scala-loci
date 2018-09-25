package loci.dev
package language
package impl
package components

import scala.collection.mutable
import scala.reflect.macros.blackbox

object Impls extends Component.Factory[Impls](
    requires = Seq(ModuleInfo, Commons, Values)) {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Impls(engine)
  def asInstance[C <: blackbox.Context] = { case c: Impls[C] => c }
}

class Impls[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq(
    Phase("impls:lift", liftNestedImplementations, before = Set("values:fixrefs"), after = Set("values:validate")))

  val moduleInfo = engine.require(ModuleInfo)
  val commons = engine.require(Commons)
  val values = engine.require(Values)

  import engine._
  import engine.c.universe._
  import moduleInfo._
  import commons._
  import values._


  def liftNestedImplementations(records: List[Any]): List[Any] = {
    (records
      flatProcess {
        case GlobalValue(symbol, owner, tree: ImplDef) =>
          val (global, nonplaced) = lift(tree)
          (global map { GlobalValue(symbol, owner, _) }) ++
          (nonplaced map { NonPlacedValue(symbol, owner, _) })
      }
      process {
        case value: Value =>
          value.copy(tree = injectPlacedValueReferences(value.tree))
      }
      process {
        case value: NonGlobalValue =>
          value.copy(tree = typeTreeLifter transform value.tree)
      })
  }

  def lift(tree: Tree): (Option[Tree], Option[Tree]) =
    lift(tree, moduleSelfReference)

  private def lift(tree: Tree, valuePrefix: Tree): (Option[Tree], Option[Tree]) =
    tree match {
      case tree: DefDef if tree.symbol.isConstructor =>
        Some(tree) -> Some(tree)
      case tree: ValOrDefDef =>
        Some(eraseValue(tree)) -> Some(tree)
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

      val (global, nonplaced) = (body map { lift(_, q"$valuePrefix.$name") }).unzip

      Some(treeCopy.ModuleDef(tree, mods, name, Template(parents, self, global.flatten))) ->
        Some(treeCopy.ModuleDef(tree, liftMods(tree.symbol, mods), name, Template(parents, self, nonplaced.flatten)))
    }
    else {
      val mods = if (tree.symbol.owner == module.classSymbol) tree.mods else liftMods(tree.symbol, tree.mods)

      Some(treeCopy.ModuleDef(tree, mods, tree.name, tree.impl)) ->
        Some(q"${moduleAliasMods(tree.mods)} val ${tree.name} = $valuePrefix.${tree.name}")
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
  private def liftMods(symbol: Symbol, mods: Modifiers, removeFlags: FlagSet = NoFlags): Modifiers = {
    val scopeRestrictionFlag =
      (mods hasFlag Flag.LOCAL) ||
      (mods hasFlag Flag.PRIVATE) ||
      (mods hasFlag Flag.PROTECTED)

    val privateWithin = liftPrivateWithin(symbol.privateWithin, scopeRestrictionFlag)

    // remove `private` and `local` flags since the cannot be used in conjunction
    // with `privateWithin` (as opposed to the `protected` flag)
    val flags =
      Seq(Flag.ABSOVERRIDE, Flag.ABSTRACT, Flag.ARTIFACT, Flag.BYNAMEPARAM,
          Flag.CASE, Flag.CASEACCESSOR, Flag.CONTRAVARIANT, Flag.COVARIANT,
          Flag.DEFAULTINIT, Flag.DEFAULTPARAM, Flag.DEFERRED, Flag.FINAL,
          Flag.IMPLICIT, Flag.INTERFACE, Flag.LAZY, Flag.MACRO, Flag.MUTABLE,
          Flag.OVERRIDE, Flag.PARAM, Flag.PARAMACCESSOR, Flag.PRESUPER,
          Flag.PROTECTED, Flag.SEALED, Flag.STABLE, Flag.SYNTHETIC, Flag.TRAIT)
        .foldLeft(NoFlags) { (flags, flag) =>
          if ((removeFlags != (removeFlags | flag)) && (mods hasFlag flag)) flags | flag else flags
        }

    Modifiers(flags, privateWithin, mods.annotations)
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

  private def enclosingPlacedValueReference(symbol: Symbol, pos: Position): Tree = {
    def enclosingUnliftedClass(symbol: Symbol): Option[ClassSymbol] =
      if (symbol == NoSymbol || symbol == module.classSymbol)
        None
      else if (symbol.isClass && !symbol.isModuleClass && isUnlifted(symbol))
        Some(symbol.asClass)
      else
        enclosingUnliftedClass(symbol.owner)

    atPos(pos) {
      (enclosingUnliftedClass(symbol)
        map { symbol => q"${symbol.name}.this.${names.placedValues.toTermName}" }
        getOrElse q"${names.placedValues}.this")
    }
  }

  // construct the stable path within a multitier module for a symbol
  // using a given prefix tree if the symbol has a stable path
  // within the module
  // module-stable implementations take part in lifting/unlifting
  // this method assumes that the instance referenced by the symbol
  // is part of this module instance
  private def moduleStablePath(symbol: Symbol, prefix: Tree): Option[Tree] =
    if ((module.symbol.info.baseClasses contains symbol) ||
        (symbol.allAnnotations exists { _.tree.tpe <:< types.multitierModule }))
      Some(prefix)
    else if (symbol.isModuleClass || symbol.isModule)
      moduleStablePath(symbol.owner, prefix) map { tree =>
        val moduleSymbol = if(symbol.isClass) symbol.asClass.module else symbol
        internal.setSymbol(Select(tree, symbol.name.toTermName), moduleSymbol)
      }
    else
      None

  // construct the stable path within a multitier module for a type
  // using a given prefix tree if the symbol has a stable path
  // within this module instance
  // module-stable implementations take part in lifting/unlifting
  private def moduleStablePath(tpe: Type, prefix: Tree): Option[Tree] = tpe match {
    case ThisType(sym) =>
      moduleStablePath(sym, prefix)
    case SingleType(_, module.symbol) =>
      Some(prefix)
    case SingleType(pre, sym) if sym.isTerm =>
      moduleStablePath(pre, prefix) map { tree =>
        internal.setSymbol(Select(tree, sym.name), sym)
      }
    case _ =>
      None
  }

  // construct the stable prefix path within a multitier module
  // consisting of multitier module instances
  private def multitierPrefixPath(tpe: Type): Option[String] = {
    def multitierPrefixPath(tree: Tree, multitier: Boolean): Option[String] = tree match {
      case EmptyTree =>
        Some("")
      case Select(qualifier, name) =>
        if (isMultitierModule(tree.symbol.info))
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
    override def transform(tree: Tree): Tree = tree match {
      case tree: TypeTree if tree.original != null =>
        internal.setOriginal(TypeTree(), transform(tree.original))
      case Select(qualifier, name) if tree.isType =>
        (moduleStablePath(qualifier.tpe, moduleSelfReference)
          map { treeCopy.Select(tree, _, name) }
          getOrElse super.transform(tree))
      case _ =>
        super.transform(tree)
    }
  }

  // create synthetic placed value references
  // potentially replacing or removing the dummy references created implicitly
  // or inserted by the preprocessor
  private def injectPlacedValueReferences(tree: Tree): Tree = {
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
                    addImplicitArgument(tree, enclosingPlacedValueReference(classSymbol.owner, classSymbol.pos))
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
                           ${enclosingPlacedValueReference(classSymbol.owner, classSymbol.pos)}""")
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
          enclosingPlacedValueReference(currentOwner, tree.pos)

        // class instantiation
        case q"new $expr[..$tpts](...$exprss)" if expr.tpe != null =>
          validateInstantiatability(expr)

          if (isUnlifted(expr.symbol) &&
              underExpansion(expr.symbol) &&
              !(skippedTrees contains expr)) {
            skippedTrees += expr
            addImplicitArgument(
              super.transform(tree),
              enclosingPlacedValueReference(currentOwner, tree.pos))
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
  private def validateInstantiatability(tree: Tree): Unit = {
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
        case _: ClassDef =>
        case _: ModuleDef =>
          moduleStackSize += 1
          super.traverse(tree)
          moduleStackSize -= 1
        case tree: ValDef
              if tree.symbol != NoSymbol &&
                 !isMultitierModule(tree.tpt.tpe) &&
                 !tree.symbol.isParameter =>
            unliftables += tree.symbol
          super.traverse(tree)
        case tree: DefDef
              if tree.symbol != NoSymbol &&
                 !isMultitierModule(tree.tpt.tpe) &&
                 !tree.symbol.isConstructor &&
                 moduleStackSize == 0 =>
            unliftables += tree.symbol
          super.traverse(tree)
        case _ =>
          super.traverse(tree)
      }
    }

    module.tree.impl.body foreach { traverser traverse _ }

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
    q"new ${trees.compileTimeOnly}($message)"
  }

  private val multitierStubAnnotation = q"new ${trees.multitierStub}"

  private def eraseValue(tree: ValOrDefDef) =
    tree map { (mods, name, _, rhs) =>
      val tpt = createTypeTree(tree.tpt)
      (mods mapAnnotations { accessAnnotation :: multitierStubAnnotation :: _ },
        name, tpt,
        if (tree.symbol.isAbstract) rhs else q"null.asInstanceOf[$tpt]")
    }
}
