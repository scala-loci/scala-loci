package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.quoted.*
import scala.quoted.runtime.StopMacroExpansion

trait PlacementContextTypesNormalization:
  this: Component with Commons with ErrorReporter with Annotations with PlacementInfo =>
  import quotes.reflect.*

  private def normalBody(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) =
    rhs map { rhs =>
      (placementInfo.peerType.asType, symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asType) match
        case ('[p], '[t]) =>
          '{ (_: Placement.Context[p]) ?=> ${rhs.asExpr}; erased: t }.asTerm.changeOwner(symbol)
    }

  private def placedExpressionSyntaxInfo(rhs: Option[Term]) =
    rhs match
      case Some(Lambda(List(arg), Apply(Apply(fun, List(Lambda(_, _))), _))) if arg.symbol.isImplicit && fun.symbol.owner == symbols.on =>
        (true, fun.symbol.name == names.sbj, true)
      case Some(Apply(Apply(fun, List(Lambda(_, _))), _)) if fun.symbol.owner == symbols.on =>
        (true, fun.symbol.name == names.sbj, false)
      case _ =>
        (false, false, false)

  private def stripPlacedExpressionSyntax(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) =
    normalBody(symbol, placementInfo, rhs map {
      case Lambda(List(arg), Apply(Apply(fun, List(Lambda(_, rhs))), _)) if arg.symbol.isImplicit && fun.symbol.owner == symbols.on => rhs
      case Apply(Apply(fun, List(Lambda(_, rhs))), _) if fun.symbol.owner == symbols.on => rhs
      case rhs => rhs
    })

  private def stripPlacementLiftingConversion(term: Term): Term =
    def stripPlacementLiftingConversion(term: Term) = term match
      case Inlined(Some(call), List(conversion: ValDef), Block(List(DefDef(names.body, List(), _, Some(rhs))), erased: Typed))
        if call.symbol == symbols.placed.companionModule &&
           conversion.tpt.tpe <:< types.conversion &&
           erased.tpe <:< types.placed =>
        rhs.underlyingArgument
      case Inlined(Some(call), List(conversion: ValDef, ValDef(_, _, Some(rhs))), Block(List(DefDef(names.body, List(), _, _)), erased: Typed))
        if call.symbol == symbols.placed.companionModule &&
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
      unliftedSubjectiveFunction: Boolean): Option[Term] =
    normalBody(symbol, placementInfo, rhs map {
      case Lambda(_, rhs) if !unliftedSubjectiveFunction =>
        stripPlacementLiftingConversion(rhs)
      case Lambda(_, lambda @ Lambda(_, rhs)) if unliftedSubjectiveFunction =>
        swapLambdaResult(lambda, placementInfo.valueType, stripPlacementLiftingConversion(rhs))
      case rhs =>
        rhs
    })

  private def noCanonicalTypeMessage(placementInfo: PlacementInfo) =
    s"Placement type should be given as: ${placementInfo.showCanonical}"

  private def noTypeInferenceMessage =
    "Placement type inference not supported with current compiler version. Type needs to be ascribed explicitly."

  private def normalizePlacementContextType(
      symbol: Symbol,
      tpt: TypeTree,
      rhs: Option[Term],
      pos: Position,
      normalizeBody: Boolean): (Option[PlacementInfo], Option[Term], Boolean) =
    val info = symbol.info
    val typeInferred =
      try tpt.pos.start == tpt.pos.end
      catch case scala.util.control.NonFatal(_) => false
    val (placedExpressionSyntax, sbjPlacedExpressionSyntax, expressionInContextFunction) = placedExpressionSyntaxInfo(rhs)
    val placementInfo = PlacementInfo(info.resultType, acceptUnliftedSubjectiveFunction = sbjPlacedExpressionSyntax && typeInferred)

    if placementInfo.isEmpty && placedExpressionSyntax then
      errorAndCancel("Placement expression for value with no placement type", pos)

    placementInfo.fold(None, rhs, false) { placementInfo =>
      if !placementInfo.canonical then
        if !typeInferred then
          errorAndCancel(noCanonicalTypeMessage(placementInfo), tpt.pos)
        SymbolMutator.get.fold(
          errorAndCancel(noTypeInferenceMessage, pos))(
          _.setInfo(symbol, info.withResultType(placementInfo.canonicalType)))

      if !placementInfo.canonical && SymbolMutator.get.isEmpty then
        (None, rhs, false)
      else if placedExpressionSyntax then
        (Some(placementInfo),
         if normalizeBody then
           stripPlacedExpressionSyntax(symbol, placementInfo, rhs)
         else
           rhs,
          placementInfo.canonical || expressionInContextFunction)
      else
        (Some(placementInfo),
         if normalizeBody then
           stripPlacementLiftingConversion(symbol, placementInfo, rhs, unliftedSubjectiveFunction = placementInfo.subjective.nonEmpty)
         else
           rhs,
         placementInfo.canonical)
    }
  end normalizePlacementContextType

  private def normalizePlacementContextType(stat: ValDef | DefDef, normalizeBody: Boolean): ValDef | DefDef = stat match
   case stat @ ValDef(name, tpt, _) =>
     val (placementInfo, rhs, _) = normalizePlacementContextType(stat.symbol, tpt, stat.rhs, stat.pos, normalizeBody)
     if normalizeBody then
       placementInfo.fold(stat) { placementInfo =>
         ValDef.copy(stat)(name, TypeTree.of(using placementInfo.canonicalType.asType), rhs)
       }
     else
       stat

   // TODO: BUG DefDef has not tpt

   // TODO: disallow using clauses for non-local placed values

   case stat @ DefDef(name, paramss, tpt, _) =>
     val (placementInfo, rhs, expressionInContextFunction) = normalizePlacementContextType(stat.symbol, tpt, stat.rhs, stat.pos, normalizeBody)
     if placementInfo.isDefined && !expressionInContextFunction then
       tryIncrementContextResultCount(stat.symbol)
     if normalizeBody then
       placementInfo.fold(stat) { placementInfo =>
         DefDef.copy(stat)(name, paramss, TypeTree.of(using placementInfo.canonicalType.asType), rhs)
       }
     else
       stat

  def normalizePlacementContextTypes(module: ClassDef): ClassDef =
    val body = module.body map {
      case ValOrDefDef(stat) =>
        normalizePlacementContextType(stat, normalizeBody = true)
      case stat =>
        stat
    }

    object contextArgumentSynthesizer extends TreeMap:
      override def transformTerm(term: Term)(owner: Symbol) = term match
        case Apply(apply @ Select(qualifier, names.apply), List(arg))
          if apply.symbol.owner == symbols.contextFunction1 &&
             arg.tpe <:< types.context =>
          transformTerm(qualifier)(owner)

        case _ =>
          val transformedTerm = super.transformTerm(term)(owner)
          val symbol = transformedTerm.symbol

          if symbol.exists && isMultitierModule(symbol.owner) then
            try symbol.tree match
              case ValOrDefDef(stat) =>
                normalizePlacementContextType(stat, normalizeBody = false)
              case _ =>
            catch
              case scala.util.control.NonFatal(_) =>

//            // TODO: there is a warning. Is it correct?
//            catch case scala.util.control.NonFatal(_) =>

            PlacementInfo(symbol.info.resultType, acceptUnliftedSubjectiveFunction = false).fold(transformedTerm) { placementInfo =>
              if !placementInfo.canonical then
                errorAndCancel(s"${noCanonicalTypeMessage(placementInfo)}${if SymbolMutator.get.isEmpty then s"\n$noTypeInferenceMessage" else ""}", term.pos)
                transformedTerm
              else
                placementInfo.peerType.asType match
                  case '[t] =>
                    Select.unique(transformedTerm, names.apply).appliedTo('{ Placement.Context.fallback[t] }.asTerm)
            }
          else
            transformedTerm
      end transformTerm
    end contextArgumentSynthesizer


    object surrogator extends SimpleTypeMap(quotes):
      override def transform(tpe: TypeRepr) =
        PlacementInfo(tpe, acceptUnliftedSubjectiveFunction = false).fold(super.transform(tpe)) { placementInfo =>
          transform(placementInfo.valueType)
        }
    end surrogator

    object contextArgumentSynthesizer2 extends TreeMap:
      override def transformTerm(term: Term)(owner: Symbol) = term match
        case Typed(expr, _) if expr.symbol.name == "erased" => term
        case _ => super.transformTerm(term)(owner)

      override def transformStatement(stat: Statement)(owner: Symbol) =
        stat match
          case ValOrDefDef(stat) => SymbolMutator.getOrErrorAndAbort.setInfo(stat.symbol, surrogator.transform(stat.symbol.info))
          case _ =>
        super.transformStatement(stat)(owner)

      override def transformTypeTree(tree: TypeTree)(owner: Symbol) =
        TypeTree.of(using surrogator.transform(tree.tpe).asType)
    end contextArgumentSynthesizer2

    object contextArgumentSynthesizer3 extends TreeMap:
      override def transformTerm(term: Term)(owner: Symbol) = term match
        case Select(qualifier @ Apply(Select(value, names.apply), _), name)
          if term.symbol.owner == symbols.placed &&
             isMultitierModule(value.symbol.owner) =>
          Select.copy(term)(transformTerm(qualifier)(owner), name)
        case _ =>
          val wrong = try term.symbol.owner == symbols.placed catch case scala.util.control.NonFatal(_) => false
          if wrong then
//            val t = Ref(Symbol.requiredMethod("loci.embedding.impl.reportError")).appliedTo(
//              Literal(StringConstant("Illegal multitier construct.")),
//              Literal(IntConstant(term.pos.start)),
//              Literal(IntConstant(term.pos.end)))
//            Block(List(t), term)
            errorAndCancel("Illegal multitier construct.", term.pos)
            term
          else
            super.transformTerm(term)(owner)
    end contextArgumentSynthesizer3

//    object myTreeTraverser extends TreeTraverser:
//      override def traverseTree(tree: Tree)(owner: Symbol): Unit = tree match
//        case tree: Term =>
//          val wrong = try tree.symbol.owner == symbols.placed catch case scala.util.control.NonFatal(_) => false
//          if wrong then
//            Block(List('{ "!!!!"; ${Expr(tree.pos.start)}; ${Expr(tree.pos.end)} }.asTerm), tree)
//          else
//            super.traverseTree(tree)(owner)
//        case _ =>
//          super.traverseTree(tree)(owner)

    ClassDef.copy(module)(module.name, module.constructor, module.parents, module.self, body map { stat =>
      contextArgumentSynthesizer.transformStatement(stat)(stat.symbol.owner) match
        case stat @ ValDef(name, tpt, Some(rhs)) =>
//          myTreeTraverser.traverseTree(rhs)(stat.symbol)
          val r = contextArgumentSynthesizer3.transformTerm(rhs)(stat.symbol)
          ValDef.copy(stat)(name, tpt, Some(r))
        case stat @ DefDef(name, paramss, tpt, Some(rhs)) =>
//          myTreeTraverser.traverseTree(rhs)(stat.symbol)
          val r = contextArgumentSynthesizer3.transformTerm(rhs)(stat.symbol)
          DefDef.copy(stat)(name, paramss, tpt, Some(r))
        case stat =>
          stat
    })
  end normalizePlacementContextTypes
end PlacementContextTypesNormalization
