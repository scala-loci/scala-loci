package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.{experimental, threadUnsafe}
import scala.collection.mutable

@experimental
trait Splitting:
  this: Component & Commons /*with ErrorReporter*/ & Annotations & PlacementInfo & PeerInfo & Synthesis & PlacementContextTypes =>
  import quotes.reflect.*

  def split(module: ClassDef): ClassDef =
    val indices = mutable.Map.empty[Symbol, Int]

    val placedBody = defn.AnyClass :: (PeerInfo.ofModule(module.symbol) map { _.peerType.typeSymbol }) map { peer =>
      val placedValues = placedValuesSymbol(module.symbol, peer)
      @threadUnsafe lazy val localDummy = SymbolMutator.getOrErrorAndAbort.createLocalDummy(placedValues)

      val body = module.body flatMap:
        case stat: ValDef if !stat.symbol.isModuleDef =>
          synthesizedVal(stat.symbol) match
            case SynthesizedDefinition.Split(_, universal, universalInit, placedInit)
                if placedValues == placedValuesSymbol(module.symbol, defn.AnyClass) =>
              List(
                ValDef(universal, Some(Ref(universalInit).appliedToNone)),
                DefDef(universalInit, _ => Some(Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(universal.info))))
            case SynthesizedDefinition.Split(_, universal, universalInit, placedInit)
                if placedInit.owner == placedValues =>

              // TODO: "universal" and "universalInit" not used: make this its own `SynthesizedDefinition` case

              List(DefDef(placedInit, _ => stat.rhs map { rhs =>
                val term = stat match
                  case PlacedStatement(_) => extractNormalizedExpression(rhs, stat.symbol)
                  case _ => rhs
                term.changeOwner(placedInit)
              }))
            case SynthesizedDefinition.Moved(_, placed)
                if placed.owner == placedValues =>
              List(ValDef(placed, stat.rhs map { rhs =>
                val term = stat match
                  case PlacedStatement(_) => extractNormalizedExpression(rhs, stat.symbol)
                  case _ => rhs
                term.changeOwner(placed)
              }))
            case _ =>
              List.empty
        case stat: DefDef =>
          val SynthesizedDefinition.Moved(_, placed) = synthesizedDef(stat.symbol)
          Option.when(placed.owner == placedValues):
            DefDef(placed, paramss =>
              stat.rhs map { rhs =>
                val term = stat match
                  case PlacedStatement(_) => extractNormalizedExpression(rhs, stat.symbol)
                  case _ => rhs
                term.changeOwner(placed).substituteRefs((stat.symbol.paramSymss.flatten zip (paramss flatMap { _ map { _.symbol } })).toMap, stat.symbol)
              })
        case PlacedStatement(term: Term) =>
          PlacementInfo(term.tpe.widenDealias.typeArgs.last).toList flatMap: placementInfo =>
            val peer = placementInfo.peerType.typeSymbol
            val index = indices.getOrElse(peer, 0)
            val name = s"<placed statement ${index} of ${fullName(peer)}>"
            indices += peer -> (index + 1)
            if placedValues == placedValuesSymbol(module.symbol, defn.AnyClass) then
              if peer == defn.AnyClass then
                List(extractNormalizedExpression(term, module.symbol).changeOwner(localDummy))
              else
                val symbol = Symbol.newMethod(placedValues, name, MethodType(List.empty)(_ => List.empty, _ => TypeRepr.of[Unit]), Flags.Synthetic, Symbol.noSymbol)
                List(
                  DefDef(symbol, _ => Some(Literal(UnitConstant()))),
                  Ref(symbol).appliedToNone)
            else if placedValues == placedValuesSymbol(module.symbol, placementInfo.peerType.typeSymbol) then
              val symbol = Symbol.newMethod(placedValues, name, MethodType(List.empty)(_ => List.empty, _ => TypeRepr.of[Unit]), Flags.Synthetic | Flags.Override, Symbol.noSymbol)
              val rhs = extractNormalizedExpression(term, module.symbol) match
                case Block(statements, expr) if expr.tpe.typeSymbol != defn.UnitClass =>
                   Block(statements :+ expr, Literal(UnitConstant()))
                case expr if expr.tpe.typeSymbol != defn.UnitClass =>
                  Block(List(expr), Literal(UnitConstant()))
                case expr =>
                  expr
              List(DefDef(symbol, _ => Some(rhs.changeOwner(symbol))))
            else
              List.empty
        case term: Term if placedValues == placedValuesSymbol(module.symbol, defn.AnyClass) =>
          Some(term.changeOwner(localDummy))
        case _ =>
          None

      val tpe = placedValues.typeRef
      val parents = tpe.baseClasses.tail map { parent => TypeTree.of(using tpe.baseType(parent).asType) }
      ClassDef(placedValues, parents, body)
    }


    def eraseBody(stat: Definition, term: Term) =
      // TODO: in any case, we want to keep all implicit functions and inject `null` into the body
      //       no distinction between placed/non-placed values
      //       maybe remove `transformNormalizedExpression`
      stat match
        case PlacedStatement(_) =>
          transformNormalizedExpression(term, stat.symbol,
            (expr, _) => expr,
            (_, _, expr) => Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(expr.tpe) -> None)
        case _ =>
          Literal(NullConstant()).select(symbols.asInstanceOf).appliedToType(stat.symbol.info.finalResultType)

    val body = module.body flatMap:
      case stat @ ValDef(name, tpt, rhs) =>
        Some(ValDef.copy(stat)(name, tpt, rhs map { eraseBody(stat, _) }))
      case stat @ DefDef(name, paramss, tpt, rhs) if !stat.symbol.isFieldAccessor =>
        Some(DefDef.copy(stat)(name, paramss, tpt, rhs map { eraseBody(stat, _) }))
      case stat: Definition =>
        Some(stat)
      case stat =>
        None

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body ++ placedBody)
  end split
end Splitting

