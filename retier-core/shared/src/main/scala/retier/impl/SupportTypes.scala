package retier
package impl

import scala.reflect.macros.whitebox.Context

private object ValueTypes {
  def Commons[C <: Context](c: C): Commons[c.type] =
    new Commons[c.type](c)

  class Commons[C <: Context](protected val c: C) {
    import c.universe._

    val controlledIssuedPlaced = typeOf[(_ <=> _) localOn _]
    val issuedPlaced = typeOf[(_ <-> _) localOn _]
    val placed = typeOf[_ localOn _]

    val remotePeer = typeOf[Remote[Peer]]

    val froms = Seq(
      typeOf[_ from _], typeOf[_ fromSingle _], typeOf[_ fromMultiple _])

    val function = typeOf[Nothing => Nothing]
    val localValueTypes = typeOf[LocalValueTypes[Nothing, Nothing]]
    val remoteValueTypes = typeOf[RemoteValueTypes[Nothing, Nothing, Nothing]]

    def replaceTypeArgs(tpe: Type, args: List[Type]) = {
      val TypeRef(pre, sym, _) = tpe
      internal typeRef (pre, sym, args)
    }
  }
}

object LocalValueTypes {
  def impl[T: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: Context): c.Expr[LocalValueTypes[T, U]] = {
    val commons = ValueTypes.Commons(c)

    import c.universe._
    import commons._

    def transform(tpe: Type) = tpe map { tpe =>
      if (tpe =:= definitions.NothingTpe)
        tpe
      else if (tpe <:< controlledIssuedPlaced)
        replaceTypeArgs(function, tpe.typeArgs.head.typeArgs)
      else if (tpe <:< issuedPlaced)
        tpe.typeArgs.head.typeArgs.last
      else if (tpe <:< placed)
        tpe.typeArgs.head
      else if (froms exists { tpe <:< _ })
        definitions.UnitTpe
      else
        tpe
    }

    val tpe = weakTypeOf[T]

    val resultType =
      replaceTypeArgs(localValueTypes, List(tpe, transform(tpe)))

    c.Expr[LocalValueTypes[T, U]](q"_root_.retier.`#macro`: $resultType")
  }
}

object RemoteValueTypes {
  def impl[T: c.WeakTypeTag, R <: Remote[Peer]: c.WeakTypeTag, U: c.WeakTypeTag]
      (c: Context): c.Expr[RemoteValueTypes[T, R, U]] = {
    val commons = ValueTypes.Commons(c)

    import c.universe._
    import commons._

    def transform(tpe: Type) = tpe map { tpe =>
      if (tpe =:= definitions.NothingTpe)
        tpe
      else if (tpe <:< controlledIssuedPlaced || tpe <:< issuedPlaced)
        tpe.typeArgs.head.typeArgs.last
      else if (tpe <:< placed)
        tpe.typeArgs.head
      else if (froms exists { tpe <:< _ })
        definitions.UnitTpe
      else
        tpe
    }

    val tpe = weakTypeOf[T]

    val remoteType =
      if (tpe =:= definitions.NothingTpe)
        definitions.NothingTpe
      else if (tpe <:< controlledIssuedPlaced || tpe <:< issuedPlaced)
        tpe.typeArgs.head.typeArgs.head
      else if (tpe <:< placed)
        remotePeer
      else
        definitions.NothingTpe

    val resultType =
      replaceTypeArgs(remoteValueTypes, List(tpe, remoteType, transform(tpe)))

    c.Expr[RemoteValueTypes[T, R, U]](q"_root_.retier.`#macro`: $resultType")
  }
}
