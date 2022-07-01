package loci
package transmitter

import utility.reflectionExtensions.*

import scala.quoted.*
import scala.util.DynamicVariable

object TransmittableResolution:
  private val optimizedTransmittableResolutionInProgress = DynamicVariable(false)

  def optimizedTransmittableResolution[B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type](using Quotes) =
    import quotes.reflect.*

    if optimizedTransmittableResolutionInProgress.value then
      report.errorAndAbort("Skipping transmittable resolution macro for recursive invocation")

    optimizedTransmittableResolutionInProgress.withValue(true) {
      val resolutionDefault = TypeRepr.of[Transmittable.ResolutionDefault].typeSymbol
      val resolutionAlternation = TypeRepr.of[Transmittable.ResolutionAlternation].typeSymbol
      val transmittable = TypeRepr.of[Transmittable.Any[?, ?, ?]].typeSymbol
      val identicallyTransmittable = TypeRepr.of[IdenticallyTransmittable[?]]
      val surrogateNothing = TypeRepr.of[Transmittable.SurrogateNothing].typeSymbol

      val transmittableParameters = (List("Base", "Intermediate", "Result", "Proxy", "Transmittables")
        map transmittable.typeMember)

      val aux = TypeIdent(TypeRepr.of[Transmittable.type].typeSymbol.typeMember("Aux")).tpe

      def boundsAsAlias(tpe: TypeRepr) = tpe match
        case TypeBounds(low, hi) if low =:= hi => low
        case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass => hi
        case TypeBounds(low, hi) if hi.typeSymbol == defn.AnyClass => low
        case _ => tpe

      def inferredOrWildcard(tpe: TypeRepr) =
        if IsInferred(tpe) then tpe else TypeBounds.empty

      object approximator extends SimpleTypeMap(quotes):
        override def transform(tpe: TypeRepr) = tpe match
          case _ if tpe.typeSymbol == defn.AnyClass || tpe.typeSymbol == defn.NothingClass =>
            TypeBounds.empty
          case tpe: AppliedType =>
            val bounds = tpe.tycon.typeSymbol.typeMembers collect { case symbol if symbol.isTypeParam => tpe.tycon.memberType(symbol) }
            if tpe.args.size == bounds.size then
              val tycon = transform(tpe.tycon)
              val args = tpe.args zip bounds map {
                case (tpe, TypeBounds(low, hi)) if tpe =:= low || tpe =:= hi => TypeBounds.empty
                case (tpe, _) => transform(tpe)
              }
              if tycon != tpe.tycon || args != tpe.args then tycon.appliedTo(args) else tpe
            else
              super.transform(tpe)
          case _ =>
            super.transform(tpe)
      end approximator

      object surrogator extends SimpleTypeMap(quotes):
        override def transform(tpe: TypeRepr) = tpe match
          case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass =>
            TypeBounds(low, super.transform(hi))
          case _ if tpe.typeSymbol == defn.NothingClass =>
            TypeRepr.of[Transmittable.SurrogateNothing]
          case _ =>
            super.transform(tpe)
      end surrogator

      val resolutionType =
        val AppliedType(tycon, List(b, i, r, p, t)) = approximator.transform(TypeRepr.of[Transmittable.Resolution[B, I, R, P, T]])
        surrogator.transform(tycon.appliedTo(List(TypeRepr.of[B], inferredOrWildcard(i), inferredOrWildcard(r), p, t)))

      Implicits.search(resolutionType) match
        case result: ImplicitSearchSuccess =>
          object deskolemizerAndTransmittablesAliaser extends SimpleTypeMap(quotes):
            override def transform(tpe: TypeRepr) = tpe match
              case _ if tpe.typeSymbol == transmittable =>
                val aliased =
                  aux.appliedTo(transmittableParameters map { param =>
                    transform(boundsAsAlias(tpe.resolvedMemberType(param)))
                  })
                if aliased != tpe then aliased else tpe
              case TypeRef(qualifier, _) if qualifier.getClass.getSimpleName contains "Skolem" =>
                val dealiased = tpe.dealias
                if dealiased != tpe then transform(dealiased) else tpe
              case _ if tpe.typeSymbol == surrogateNothing =>
                TypeRepr.of[Nothing]
              case _ =>
                super.transform(tpe)
          end deskolemizerAndTransmittablesAliaser

          object optimizer extends TreeMap:
            override def transformTypeTree(tree: TypeTree)(owner: Symbol) =
              val tpe = deskolemizerAndTransmittablesAliaser.transform(tree.tpe)
              if tpe != tree.tpe then TypeTree.of(using tpe.asType) else tree

            override def transformTerm(tree: Term)(owner: Symbol) = tree match
              case Apply(TypeApply(fun, _), List(_, _, arg))
                  if fun.symbol.owner == resolutionDefault || fun.symbol.owner == resolutionAlternation =>
                val tpe = arg.tpe.widenTermRefByName
                val args = transmittableParameters map { param =>
                  deskolemizerAndTransmittablesAliaser.transform(boundsAsAlias(tpe.resolvedMemberType(param)))
                }
                val Apply(TypeApply(resolution, _), _) = '{ Transmittable.Resolution(???) }.asTerm.underlying

                resolution.appliedToTypes(args).appliedTo(transformTerm(arg)(owner))

              case _
                if tree.tpe.typeSymbol != defn.NullClass &&
                   tree.tpe.typeSymbol != defn.NothingClass &&
                   tree.tpe <:< identicallyTransmittable =>
                val AppliedType(_, List(tpe)) = tree.tpe.widenTermRefByName.dealias
                val arg = deskolemizerAndTransmittablesAliaser.transform(tpe)
                val Apply(TypeApply(transmittable, _), _) = '{ IdenticallyTransmittable[Any]() }.asTerm.underlying

                transmittable.appliedToType(arg).appliedToNone

              case _ =>
                super.transformTerm(tree)(owner)
          end optimizer

          optimizer.transformTree(result.tree)(Symbol.spliceOwner).asExpr match
            case result: Expr[Transmittable.Resolution[B, I, R, P, T]] @unchecked => result
    }
  end optimizedTransmittableResolution

  def delegatingResolution[D <: Transmittable.Delegating: Type](using Quotes) =
    import quotes.reflect.*

    def summon[T: Type](using Quotes): Expr[T] =
      val transmittable =
        Type.of[T] match
          case '[ Transmittable.Aux[b, i, r, p, t] ] =>
            Expr.summon[Transmittable.Resolution[b, i, r, p, t]] map { resolution =>
              '{$resolution.transmittable}.asExprOf[T]
            }
          case _ =>
            None

      transmittable getOrElse report.errorAndAbort("Delegation is not transmittable")

    def resolve[D: Type](using Quotes): Expr[D] =
      Type.of[D] match
        case '[ d / t ] => '{ /(${resolve[d]}, ${summon[t]}) }.asExprOf[D]
        case _ => summon[D]

    '{ Transmittable.Delegating.Resolution(${resolve[D]}) }
  end delegatingResolution
end TransmittableResolution
