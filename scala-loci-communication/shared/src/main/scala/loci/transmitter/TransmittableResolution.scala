package loci
package transmitter

import scala.reflect.macros.whitebox

object TransmittableResolution {
  def apply[
      B: c.WeakTypeTag,
      I: c.WeakTypeTag,
      R: c.WeakTypeTag,
      P: c.WeakTypeTag,
      T: c.WeakTypeTag](c: whitebox.Context)(dummy: c.Tree): c.Tree = {
    import c.universe._

    val B = weakTypeOf[B]
    val I = weakTypeOf[I]
    val R = weakTypeOf[R]
    val P = weakTypeOf[P]
    val T = weakTypeOf[T]

    val resolutionModule = symbolOf[Transmittable.Aux.Resolution.type]
    if (B =:= definitions.NothingTpe || B.typeSymbol.owner.owner == resolutionModule)
      c.abort(c.enclosingPosition,
        "Skipping transmittable resolution macro to resolve implicit value for `Nothing` parameter")

    // construct `Transmittable.DependantValue[B, I, R, Transmittable.Aux[B, I, R, P, T]]` type for implicit resolution
    // replace type parameters of macro application that are not inferred with existentials
    // replace `Nothing` types with different `Transmittable.Bottom` types and remember the corresponding original type
    def createResolutionType = {
      val TypeRef(bottomPre, bottomSym, _) =
        typeOf[Transmittable.Bottom[Any]]: @unchecked
      val TypeRef(singletonPre, singletonSym, _) =
        typeOf[Transmittable.DependantValue[Any, Any, Any, Any]]: @unchecked
      val ExistentialType(existentialQuantified, TypeRef(auxPre, auxSym, existentialArgs)) =
        typeOf[Transmittable.Aux[_, _, _, _, _]]: @unchecked

      var quantified = List.empty[Symbol]
      var args = List.empty[Type]
      var originals = List.empty[(Type, Type)]
      var bottom = definitions.UnitTpe

      def createArg(tpe: Type, index: Int) =
        if (tpe.typeSymbol.owner.owner == resolutionModule) {
          quantified ::= existentialQuantified(index)
          args ::= existentialArgs(index)
        }
        else
          args ::= tpe map {
            case tpe if tpe =:= definitions.NothingTpe =>
              bottom = internal.typeRef(bottomPre, bottomSym, List(bottom))
              originals ::= bottom -> tpe
              bottom
            case tpe =>
              tpe
          }

      List(T, P, R, I, B).zipWithIndex foreach (createArg _).tupled

      val typeRef = internal.typeRef(singletonPre, singletonSym,
        (args take 3) :+ internal.typeRef(auxPre, auxSym, args))

      if (quantified.isEmpty)
        typeRef -> originals
      else
        internal.existentialType(quantified, typeRef) -> originals
    }

    val (resolutionType, originals) = createResolutionType


    // resolve `Transmittable.DependantValue[B, I, R, Transmittable.Aux[B, I, R, P, T]]` value
    // extract `Transmittable` instance
    val resolutionTree = c inferImplicitValue resolutionType match {
      case q"$_[..$_]($expr)" => expr
      case _ => EmptyTree
    }

    if (resolutionTree.isEmpty)
      c.abort(c.enclosingPosition,
        "Skipping transmittable resolution macro due to unresolved implicit")


    // restore original types for types replaced with `Transmittable.Bottom` types
    // contract `IdenticallyTransmittable` instances
    object transformer extends Transformer {
      val identicallyTransmittableType = typeOf[IdenticallyTransmittable[_]]
      val identicallyTransmittableTree = q"${termNames.ROOTPKG}.loci.transmitter.IdenticallyTransmittable"

      def hasNonRepresentableType(tree: Tree): Boolean =
        tree.tpe != null &&
        (tree.tpe exists { tpe =>
          val name = tpe.typeSymbol.name.toString
          name == "<refinement>" || (name endsWith ".type")
        })

      def originalType(tpe: Type): Option[Type] =
        originals collectFirst { case (bottom, original) if tpe =:= bottom => original }

      def restoreType(tpe: Type): Type =
        tpe map { tpe => originalType(tpe) getOrElse tpe }

      def restoreType(tree: Tree): Tree =
        if (hasNonRepresentableType(tree))
          internal.setType(tree, null)
        else if (tree.tpe != null)
          internal.setType(tree, restoreType(tree.tpe))
        else
          tree

      override def transform(tree: Tree): Tree = tree match {
        case tree
          if tree.symbol != null &&
             tree.tpe != null &&
             tree.symbol.isMethod &&
             tree.tpe <:< identicallyTransmittableType =>
          q"$identicallyTransmittableTree[${restoreType(tree.tpe.typeArgs.head)}]()"

        case TypeApply(fun, args) if args exists hasNonRepresentableType =>
          super.transform(fun)

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          super.transform(
            restoreType(
              treeCopy.DefDef(tree,
                mods mapAnnotations { (tree.symbol.annotations map { _.tree }) ++ _ },
                name, tparams, vparamss, tpt, rhs)))

        case _ =>
          super.transform(restoreType(tree))
      }
    }

    val result = c untypecheck (transformer transform resolutionTree)

    // construct `Transmittable.Aux.Resolution` instance
    // and type-check against the expected type
    def createExpectedResolutionType = {
      val ExistentialType(existentialQuantified, TypeRef(pre, sym, existentialArgs)) =
        typeOf[Transmittable.Aux.Resolution[_, _, _, _, _]]: @unchecked

      var quantified = List.empty[Symbol]
      var args = List.empty[Type]

      def createArg(tpe: Type, index: Int) =
        if (tpe.typeSymbol.owner.owner == resolutionModule) {
          quantified ::= existentialQuantified(index)
          args ::= existentialArgs(index)
        }
        else
          args ::= tpe

      List(T, P, R, I, B).zipWithIndex foreach (createArg _).tupled

      val typeRef = internal.typeRef(pre, sym, args)

      if (quantified.isEmpty)
        typeRef
      else
        internal.existentialType(quantified, typeRef)
    }

    c.typecheck(
      q"new ${termNames.ROOTPKG}.loci.transmitter.Transmittable.Aux.Resolution($result)",
      pt = createExpectedResolutionType)
  }
}
