package loci.dev
package language
package impl
package components

import scala.reflect.macros.blackbox

object Commons extends Component.Factory[Commons] {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Commons(engine)
  def asInstance[C <: blackbox.Context] = { case c: Commons[C] => c }
}

class Commons[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  import engine.c.universe._
  import names._

  object names {
    val root = termNames.ROOTPKG
    val tie = TypeName("Tie")
  }

  object symbols {
    val on = symbolOf[_ on _]
    val per = symbolOf[_ per _]
    val local = symbolOf[Local[_]]
    val On = symbolOf[Placement.On[_]]
    val Placed = symbolOf[Placement.Placed]
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
  }

  object trees {
    val compileTimeOnly = tq"$root.scala.annotation.compileTimeOnly"
    val remote = tq"$root.loci.dev.Remote"
  }

  def uniqueName(symbol: Symbol): String = {
    val owner = symbol.owner
    val name = symbol.name.toString

    if (owner == engine.c.mirror.RootClass)
      name
    else if (symbol.isSynthetic)
      uniqueName(owner)
    else {
      val prefix = uniqueName(owner)
      val separator = if (owner.isType && !owner.isModuleClass) "$$$" else "$"
      val suffix = if (name endsWith termNames.LOCAL_SUFFIX_STRING) name.dropRight(1) else name
      s"$prefix$separator$suffix"
    }
  }

  def uniqueName(tpe: Type, outer: Type): String = tpe match {
    case TypeRef(ThisType(_), sym, _) =>
      if (outer.members exists { _ == sym }) sym.name.toString else uniqueName(sym)
    case SingleType(ThisType(_), sym) =>
      if (outer.members exists { _ == sym }) sym.name.toString else uniqueName(sym)
    case TypeRef(pre, sym, _) =>
      s"${uniqueName(pre, outer)}$$${sym.name.toString}"
    case SingleType(pre, sym) =>
      s"${uniqueName(pre, outer)}$$${sym.name.toString}"
    case _ => uniqueName(tpe.typeSymbol)
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

  implicit class TreeOps(tree: Tree) {
    def original: Tree = tree match {
      case tree: TypeTree => tree.original
      case tree => tree
    }
  }

  implicit class ValOrDefDefOps(tree: ValOrDefDef) {
    def map(f: (Modifiers, TermName, Tree, Tree) => (Modifiers, TermName, Tree, Tree)): ValOrDefDef = tree match {
      case ValDef(mods, name, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        internal.setPos(
          ValDef(modsNew, nameNew, tptNew, rhsNew),
          tree.pos)

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        internal.setPos(
          DefDef(modsNew, nameNew, tparams, vparamss, tptNew, rhsNew),
          tree.pos)
    }
  }
}
