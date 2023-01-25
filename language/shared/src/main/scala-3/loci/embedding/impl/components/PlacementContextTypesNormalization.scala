package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.quoted.*

trait PlacementContextTypesNormalization:
  this: Component with Commons with Annotations with PlacementInfo =>
  import quotes.reflect.*

  private def symbolMutator(pos: Position) = SymbolMutator.get getOrElse
    report.errorAndAbort("Placement type inference not supported with current compiler version. Type needs to be ascribed explicitly.", pos)

  private def normalBody(symbol: Symbol, placementInfo: PlacementInfo, rhs: Term) =
    (placementInfo.peerType.asType, symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asType) match
      case ('[p], '[t]) =>
        given Quotes = symbol.asQuotes
        '{ (_: Placement.Context[p]) ?=> ${rhs.asExpr}; erased: t }.asTerm

  private def placedExpressionSyntaxInfo(rhs: Option[Term]) =
    rhs match
      case Some(Lambda(List(arg), Apply(Apply(fun, List(Lambda(_, _))), _))) if arg.symbol.isImplicit && fun.symbol.owner == symbols.on =>
        (true, fun.symbol.name == names.sbj)
      case Some(Apply(Apply(fun, List(Lambda(_, _))), _)) if fun.symbol.owner == symbols.on =>
        (true, fun.symbol.name == names.sbj)
      case _ =>
        (false, false)

  private def stripPlacedExpressionSyntax(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) =
    rhs match
      case Some(Lambda(List(arg), Apply(Apply(fun, List(Lambda(_, rhs))), _))) if arg.symbol.isImplicit && fun.symbol.owner == symbols.on =>
        Some(normalBody(symbol, placementInfo, rhs)) -> false
      case Some(Apply(Apply(fun, List(Lambda(_, rhs))), _)) if fun.symbol.owner == symbols.on =>
        Some(normalBody(symbol, placementInfo, rhs)) -> true
      case Some(rhs) =>
        Some(normalBody(symbol, placementInfo, rhs)) -> true
      case _ =>
         None -> true

  private def stripPlacementLiftingConversion(term: Term): Term =
    def stripPlacementLiftingConversion(term: Term) = term match
      case Inlined(Some(call), List(conversion: ValDef), Block(List(DefDef(names.body, List(), _, Some(rhs))), erased: Typed))
        if call.symbol == symbols.placed &&
           conversion.tpt.tpe <:< types.conversion &&
           erased.tpe <:< types.placed =>
        rhs.underlyingArgument
      case Inlined(Some(call), List(conversion: ValDef, ValDef(_, _, Some(rhs))), Block(List(DefDef(names.body, List(), _, _)), erased: Typed))
        if call.symbol == symbols.placed &&
           conversion.tpt.tpe <:< types.conversion &&
           erased.tpe <:< types.placed =>
        rhs.underlyingArgument
      case _ =>
        term

    term match
      case Block(statements, expr) => Block.copy(term)(statements, stripPlacementLiftingConversion(expr))
      case _ => stripPlacementLiftingConversion(term)
  end stripPlacementLiftingConversion

  private def stripPlacementLiftingConversion(
      symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term],
      unliftedSubjectiveFunction: Boolean): Option[Term] = rhs match
    case Some(Lambda(_, rhs)) if !unliftedSubjectiveFunction =>
      Some(normalBody(symbol, placementInfo, stripPlacementLiftingConversion(rhs)))
    case Some(Lambda(_, lambda @ Lambda(_, rhs))) if unliftedSubjectiveFunction =>
      Some(normalBody(symbol, placementInfo, swapLambdaResult(lambda, placementInfo.valueType, stripPlacementLiftingConversion(rhs))))
    case _ =>
      rhs

  private def normalizePlacementContextType(symbol: Symbol, tpt: TypeTree, rhs: Option[Term], pos: Position) =
    val info = symbol.info
    val typeInferred = tpt.pos.start == tpt.pos.end
    val (placedExpressionSyntax, sbjPlacedExpressionSyntax) = placedExpressionSyntaxInfo(rhs)
    val placementInfo = PlacementInfo(info.resultType, acceptUnliftedSubjectiveFunction = sbjPlacedExpressionSyntax && typeInferred)

    if placementInfo.isEmpty && placedExpressionSyntax then
      report.errorAndAbort("Placement expression for value with no placement type", pos)

    placementInfo.fold(None, rhs, false) { placementInfo =>
      if !placementInfo.canonical then
        if !typeInferred then
          report.errorAndAbort(s"Placement type should be given as: ${placementInfo.showCanonical}", tpt.pos)
        symbolMutator(pos).setInfo(symbol, info.withResultType(placementInfo.canonicalType))

      if placedExpressionSyntax then
        Some(placementInfo) *:
        stripPlacedExpressionSyntax(symbol, placementInfo, rhs)
      else
        (Some(placementInfo),
         stripPlacementLiftingConversion(symbol, placementInfo, rhs, unliftedSubjectiveFunction = placementInfo.subjective.nonEmpty),
         false)
    }
  end normalizePlacementContextType

  def normalizePlacementContextTypes(module: ClassDef): ClassDef =
    val body = module.body map {
      case stat @ ValDef(name, tpt, _) =>
        val (placementInfo, rhs, _) = normalizePlacementContextType(stat.symbol, tpt, stat.rhs, stat.pos)
        placementInfo.fold(stat) { placementInfo =>
          ValDef.copy(stat)(name, TypeTree.of(using placementInfo.canonicalType.asType), rhs)
        }

      // TODO: BUG DefDef has not tpt

      case stat @ DefDef(name, paramss, tpt, _) =>
        val (placementInfo, rhs, incrementContextResultCount) = normalizePlacementContextType(stat.symbol, tpt, stat.rhs, stat.pos)
        placementInfo.fold(stat) { placementInfo =>
          if incrementContextResultCount then
            tryIncrementContextResultCount(stat.symbol)
          DefDef.copy(stat)(name, paramss, TypeTree.of(using placementInfo.canonicalType.asType), rhs)
        }
      case stat =>
        stat
    }

    object myTreeMap extends TreeMap:
      override def transformTerm(term: Term)(owner: Symbol) = term match
        case Ident(name) if name.startsWith("evidence") =>
          term.tpe.widenTermRefByName.typeArgs.head.asType match
            case '[ t ] =>
              '{ Placement.Context.fallback[t] }.asTerm
        case _ =>
          super.transformTerm(term)(owner)

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body map { stat =>
      myTreeMap.transformStatement(stat)(stat.symbol.owner)
    })
  end normalizePlacementContextTypes
end PlacementContextTypesNormalization
