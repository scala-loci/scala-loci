package loci.dev
package language
package impl

import scala.reflect.macros.blackbox.Context

trait Definitions {
  val c: Context

  import c.universe._
  import names._

  object names {
    val root = termNames.ROOTPKG
    val tie = TypeName("Tie")
  }

  object symbols {
    val on = symbolOf[_ on _]
    val per = symbolOf[_ per _]
    val local = symbolOf[Local[_]]
  }

  object types {
    val multiple = typeOf[Multiple[_]]
    val optional = typeOf[Optional[_]]
    val single = typeOf[Single[_]]
    val peer = typeOf[peer]
  }

  object trees {
    val compileTimeOnly = tq"$root.scala.annotation.compileTimeOnly"
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
        ValDef(modsNew, nameNew, tptNew, rhsNew)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        DefDef(modsNew, nameNew, tparams, vparamss, tptNew, rhsNew)
    }
  }
}
