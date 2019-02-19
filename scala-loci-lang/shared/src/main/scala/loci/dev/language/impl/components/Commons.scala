package loci.dev
package language
package impl
package components

import retypecheck._

import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox

object Commons extends Component.Factory[Commons] {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Commons(engine)
  def asInstance[C <: blackbox.Context] = { case c: Commons[C] => c }
}

class Commons[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  import engine.c.universe._

  val retyper = engine.c.retyper

  def expandMultitierModule(tree: ImplDef): ImplDef = {
    val result = engine.run(tree)
    val assembly = result.engine.require(Assembly)
    result.records collectFirst { case assembly.Assembly(tree) => tree } getOrElse tree
  }

  object names {
    val root = termNames.ROOTPKG
    val tie = TypeName("Tie")
    val base = TypeName("Base")
    val intermediate = TypeName("Intermediate")
    val result = TypeName("Result")
    val proxy = TypeName("Proxy")
    val transmittables = TypeName("Transmittables")
    val placedValues = TypeName(NameTransformer encode "<placed values>")
    val multitierModule = TermName(NameTransformer encode "<multitier module>")
  }

  object symbols {
    val on = symbolOf[_ on _]
    val per = symbolOf[_ per _]
    val local = symbolOf[Local[_]]
    val On = symbolOf[Placement.On[_]]
    val Placed = symbolOf[Placement.Placed]
    val placedValues = engine.c.mirror.staticModule("_root_.loci.dev.runtime.PlacedValues")
  }

  object types {
    val function = typeOf[_ => _]
    val multiple = typeOf[Multiple[_]]
    val optional = typeOf[Optional[_]]
    val single = typeOf[Single[_]]
    val peer = typeOf[peer]
    val remote = typeOf[Remote[_]]
    val placedValue = typeOf[PlacedValue[_, _]]
    val subjective = typeOf[Placed.Subjective[_, _]]
    val multitierStub = typeOf[runtime.MultitierStub]
    val multitierModule = typeOf[runtime.MultitierModule]
    val marshallableInfo = typeOf[runtime.MarshallableInfo[_]]
    val placedRuntimeValue = typeOf[runtime.PlacedValue[_, _, _, _, _, _]]
    val placedRuntimeValueInfo = typeOf[runtime.PlacedValueInfo]
    val remoteValue = typeOf[runtime.RemoteValue[_, _]]
    val marshallable = typeOf[loci.transmitter.Marshallable[_, _, _]]
    val transmittable = typeOf[loci.transmitter.Transmittable[_, _, _]]
    val resolution = typeOf[loci.transmitter.Transmittable.Aux.Resolution[_, _, _, _, _]]
    val serializable = typeOf[loci.transmitter.Serializable[_]]
    val transmission = typeOf[transmitter.Transmission[_, _, _, _]]
    val accessor = typeOf[transmitter.RemoteAccessor]
    val delegates = typeOf[loci.transmitter.Transmittables.Delegates[_ ]]
    val message = typeOf[loci.transmitter.Transmittables.Message[_]]
    val none = typeOf[loci.transmitter.Transmittables.None]
    val compileTimeOnly = typeOf[annotation.compileTimeOnly]
    val placedValues = symbols.placedValues.companion.asType.toType
  }

  object trees {
    val implicitly = q"${names.root}.scala.Predef.implicitly"
    val marshallable = q"${names.root}.loci.transmitter.Marshallable.marshallable"
    val resolution = q"${names.root}.loci.transmitter.Transmittable.Aux.resolution"
    val delegating = q"${names.root}.loci.transmitter.ContextBuilder.delegating"
    val messaging = q"${names.root}.loci.transmitter.ContextBuilder.messaging"
    val none = q"${names.root}.loci.transmitter.ContextBuilder.none"
    val delegate = q"${names.root}.loci.transmitter.ContextBuilders.delegate"
    val list = q"${names.root}.loci.transmitter.ContextBuilders.list"
  }

  def createTypeTree(tpe: Type, pos: Position): Tree = {
    def containsTypeTree(tree: Tree) = tree exists {
      case _: TypeTree => true
      case _ => false
    }

    val tree = retyper.createTypeTree(tpe, pos)
    if (containsTypeTree(tree)) {
      val underlyingTree = retyper.createTypeTree(tpe.underlying, pos)
      if (containsTypeTree(underlyingTree))
        tree
      else
        underlyingTree
    }
    else
      tree
  }

  def createTypeTree(tree: Tree): Tree =
    tree match {
      case tree: TypeTree if tree.original == null =>
        createTypeTree(tree.tpe, tree.pos)
      case _ =>
        tree
    }

  def uniqueName(symbol: Symbol): String = {
    val owner = symbol.owner
    val name = symbol.name.toString

    if (owner == engine.c.mirror.RootClass)
      name
    else if (symbol.isSynthetic || ((name startsWith "<") && (name endsWith ">")))
      uniqueName(owner)
    else {
      val prefix = uniqueName(owner)
      val separator = if (owner.isType && !owner.isModuleClass) "$$$" else "$"
      val suffix = if (name endsWith termNames.LOCAL_SUFFIX_STRING) name.dropRight(1) else name
      s"$prefix$separator$suffix"
    }
  }

  implicit class ListOps[T](list: List[T]) {
    def process(f: PartialFunction[T, T]): List[T] =
      list map { v => f.applyOrElse(v, identity[T]) }
    def flatProcess(f: PartialFunction[T, TraversableOnce[T]]): List[T] =
      list flatMap { v => f.applyOrElse(v, Traversable(_: T)) }
  }

  implicit class TypeOps(tpe: Type) {
    def =:!=(other: Type): Boolean = !(tpe =:= other)

    def <:!<(other: Type): Boolean = !(tpe <:< other)

    def real_<:!<(other: Type): Boolean = !(tpe real_<:< other)

    def real_<:<(other: Type): Boolean = tpe != null && tpe <:< other &&
      tpe <:!< definitions.NothingTpe && tpe <:!< definitions.NullTpe

    def underlying: Type =
      if (tpe ne tpe.dealias)
        tpe.dealias.underlying
      else if (tpe ne tpe.widen)
        tpe.widen.underlying
      else
        tpe

    def mapArgs(f: List[Type] => List[Type]): Type = tpe match {
      case ExistentialType(quantified, TypeRef(pre, sym, args)) =>
        val newArgs = f(args)
        val newQuantified = quantified filter { symbol =>
          newArgs exists { _ contains symbol }
        }
        val newUnderlying = internal.typeRef(pre, sym, newArgs)
        if (newQuantified.nonEmpty)
          internal.existentialType(newQuantified, newUnderlying)
        else
          newUnderlying
      case TypeRef(pre, sym, args) =>
        internal.typeRef(pre, sym, f(args))
      case _ =>
        tpe
    }
  }

  implicit class SymbolOps(symbol: Symbol) {
    def allAnnotations: List[Annotation] =
      if (symbol.isMethod && symbol.asMethod.isAccessor)
        symbol.annotations ++ symbol.asMethod.accessed.annotations
      else
        symbol.annotations
    def fullNestedName: String = {
      val name = symbol.fullName
      val tpe =
        if (symbol.isType)
          symbol.asType.toType
        else
          NoType

      (Seq(
          definitions.UnitTpe, definitions.ByteTpe, definitions.ShortTpe, definitions.CharTpe,
          definitions.IntTpe, definitions.LongTpe, definitions.FloatTpe, definitions.DoubleTpe,
          definitions.BooleanTpe, definitions.AnyTpe, definitions.AnyValTpe, definitions.AnyRefTpe,
          definitions.ObjectTpe, definitions.NothingTpe, definitions.NullTpe)
        collectFirst {
          case standardTpe if tpe =:= standardTpe => standardTpe.toString
        }
        getOrElse {
          val index = if (name startsWith "scala.") -1 else name lastIndexOf "."
          if (index > 0)
            s"${name.substring(index + 1)} in ${name.substring(0, index)}"
          else
            name
        })
    }
  }

  implicit class PositionOps(pos: Position) {
    def orElse(other: Position): Position = if (pos == NoPosition) other else pos
  }

  implicit class ModifiersOps(mods: Modifiers) {
    def withFlags(flags: FlagSet): Modifiers =
      Modifiers(mods.flags | flags, mods.privateWithin, mods.annotations)
    def withoutFlags(flags: FlagSet): Modifiers = {
      val reducedFlags =
        Seq(Flag.ABSOVERRIDE, Flag.ABSTRACT, Flag.ARTIFACT, Flag.BYNAMEPARAM,
            Flag.CASE, Flag.CASEACCESSOR, Flag.CONTRAVARIANT, Flag.COVARIANT,
            Flag.DEFAULTINIT, Flag.DEFAULTPARAM, Flag.DEFERRED, Flag.FINAL,
            Flag.IMPLICIT, Flag.INTERFACE, Flag.LAZY, Flag.LOCAL, Flag.MACRO,
            Flag.MUTABLE, Flag.OVERRIDE, Flag.PARAM, Flag.PARAMACCESSOR,
            Flag.PRESUPER, Flag.PRIVATE, Flag.PROTECTED, Flag.SEALED,
            Flag.STABLE, Flag.SYNTHETIC, Flag.TRAIT)
          .foldLeft(NoFlags) { (flagAcc, flag) =>
            if ((flags != (flags | flag)) && (mods hasFlag flag)) flagAcc | flag else flagAcc
          }
      Modifiers(reducedFlags, mods.privateWithin, mods.annotations)
    }
  }

  implicit class TreeOps(tree: Tree) {
    def original: Tree = tree match {
      case tree: TypeTree => tree.original
      case tree => tree
    }

    def fullyExpanded: Tree =
      typeTreeExpander transform tree
  }

  implicit class ImplDefOps(tree: ImplDef) {
    def map(f: (Modifiers, List[Tree], ValDef, List[Tree]) =>
               (Modifiers, List[Tree], ValDef, List[Tree])): ImplDef = tree match {
      case ClassDef(mods, tpname, tparams, impl @ Template(parents, self, body)) =>
        val (modsNew, parentsNew, selfNew, bodyNew) = f(mods, parents, self, body)
        treeCopy.ClassDef(tree, modsNew, tpname, tparams,
          treeCopy.Template(impl, parentsNew, selfNew, bodyNew))

      case ModuleDef(mods, tpname, impl @ Template(parents, self, body)) =>
        val (modsNew, parentsNew, selfNew, bodyNew) = f(mods, parents, self, body)
        treeCopy.ModuleDef(tree, modsNew, tpname,
          treeCopy.Template(impl, parentsNew, selfNew, bodyNew))
    }
  }

  implicit class ValOrDefDefOps(tree: ValOrDefDef) {
    def map(f: (Modifiers, TermName, Tree, Tree) =>
               (Modifiers, TermName, Tree, Tree)): ValOrDefDef = tree match {
      case ValDef(mods, name, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        treeCopy.ValDef(tree, modsNew, nameNew, tptNew, rhsNew)

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        treeCopy.DefDef(tree, modsNew, nameNew, tparams, vparamss, tptNew, rhsNew)
    }
  }

  private object typeTreeExpander extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case tree: TypeTree =>
        retyper.createTypeTree(tree.tpe, tree.pos) match {
          case tree: TypeTree => tree
          case tree => transform(tree)
        }
      case _ =>
        super.transform(tree)
    }
  }
}
