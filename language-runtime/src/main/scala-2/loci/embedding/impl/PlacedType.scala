package loci
package embedding
package impl

import language._

import scala.reflect.macros.whitebox

object PlacedType {
  def apply[
      V: c.WeakTypeTag, L: c.WeakTypeTag, T: c.WeakTypeTag](
      c: whitebox.Context)(ev: c.Tree): c.Tree = {
    import c.universe._

    val local = symbolOf[Local[_]]
    val placedValue = typeOf[PlacedValue[_, _]]
    val placed = typeOf[Placed[_, _]]
    val subjective = typeOf[Placed.Subjective[_, _]]
    val placedClean = symbolOf[PlacedClean.type]
    val erased = q"${termNames.ROOTPKG}.loci.embedding.erased"

    val v = weakTypeOf[V]
    val l = weakTypeOf[L]
    val t = weakTypeOf[T]

    if (t.typeSymbol.isParameter && t.typeSymbol.owner.owner == placedClean)
      c.abort(c.enclosingPosition, "")


    def placedCleanType(p: Type, l: Type, t: Type, u: Type): Type = {
      val TypeRef(pre, sym, _) = typeOf[PlacedClean[Any, Any, Any, Any, Any]]: @unchecked
      internal.typeRef(pre, sym, List(p, l, t, t, u))
    }

    def on(t: Type, p: Type): Type = {
      val TypeRef(pre, sym, _) = typeOf[Any on Any]: @unchecked
      internal.typeRef(pre, sym, List(t, p))
    }

    def per(t: Type, p: Type): Type = {
      val ExistentialType(_, TypeRef(pre, sym, _)) = subjective: @unchecked
      internal.typeRef(pre, sym, List(t, p))
    }


    def underlying(tpe: Type): Type =
      if (tpe ne tpe.dealias)
        underlying(tpe.dealias)
      else if (tpe ne tpe.widen)
        underlying(tpe.widen)
      else
        tpe

    def placedType(tpe: Type) =
      underlying(tpe) match {
        case RefinedType(Seq(valuePeer, value), _)
            if valuePeer <:< placed &&
               !(valuePeer <:< definitions.NothingTpe) &&
               !(valuePeer <:< definitions.NullTpe) =>
          val Seq(placed, peer) = valuePeer.typeArgs: @unchecked
          if (value =:= placed)
            Some(placed -> peer)
          else
            None
        case _ =>
          None
      }

    def subjectiveType(tpe: Type) =
      underlying(tpe) match {
        case tpe
            if tpe <:< subjective &&
               !(tpe <:< definitions.NothingTpe) &&
               !(tpe <:< definitions.NullTpe) =>
          val Seq(value, peer) = tpe.typeArgs: @unchecked
          Some(value -> peer)
        case _ =>
          None
      }


    def clean(tpe: Type, localPeer: Type): Type = {
      def placed =
        placedType(tpe) map { case (value, peer) =>
          if (subjectiveType(value).isEmpty && localPeer <:< peer)
            clean(value, localPeer)
          else
            definitions.UnitTpe
        }

      def subjective =
        subjectiveType(tpe) map { case (value, peer) =>
          per(clean(value, definitions.NothingTpe), peer)
        }

      def selection =
        if (tpe <:< placedValue &&
            !(tpe <:< definitions.NothingTpe) &&
            !(tpe <:< definitions.NullTpe))
          Some(definitions.UnitTpe)
        else
          None

      placed orElse subjective orElse selection getOrElse tpe match {
        case TypeRef(_, `local`, Seq(value)) =>
          clean(value, localPeer)
        case AnnotatedType(annotations, underlying) =>
          internal.annotatedType(annotations, clean(underlying, localPeer))
        case BoundedWildcardType(TypeBounds(lo, hi)) =>
          internal.boundedWildcardType(internal.typeBounds(clean(lo, localPeer), clean(hi, localPeer)))
        case ExistentialType(quantified, underlying) =>
          internal.existentialType(quantified, clean(underlying, localPeer))
        case RefinedType(parents, scope) =>
          internal.refinedType(parents map { clean(_, localPeer) }, scope)
        case SingleType(pre, sym) =>
          internal.singleType(clean(pre, localPeer), sym)
        case TypeBounds(lo, hi) =>
          internal.typeBounds(clean(lo, localPeer), clean(hi, localPeer))
        case TypeRef(pre, sym, args) =>
          internal.typeRef(clean(pre, localPeer), sym, args map { clean(_, localPeer) })
        case tpe =>
          tpe
      }
    }


    if (v.typeSymbol.isParameter) {
      val v = clean(t, l)
      q"$erased: ${placedCleanType(on(v, l), l, t, v)}"
    }
    else
      q"$erased: ${placedCleanType(on(v, l), l, t, v)}"
  }
}
