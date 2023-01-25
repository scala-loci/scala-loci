package loci
package embedding
package impl

import embedding.*
import utility.reflectionExtensions.*

import scala.annotation.threadUnsafe
import scala.quoted.*

object Multitier:
  def annotation(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    tree match
      case tree: ClassDef =>
        List(normalizePlacementContextTypes(tree))
      case _ =>
        report.errorAndAbort("multitier annotation only applicable to classes, traits or objects")

def normalizePlacementContextTypes(using Quotes)(module: quotes.reflect.ClassDef): quotes.reflect.ClassDef =
  import quotes.reflect.*

  object symbols:
    val per = Symbol.requiredPackage("loci.language.package$package").typeMember("per")
    val `language.on` = Symbol.requiredPackage("loci.language.package$package").typeMember("on")
    val `embedding.on` = Symbol.requiredPackage("loci.embedding.package$package").typeMember("on")
    val `Placed.on` = Symbol.requiredPackage("loci.embedding.Placed").typeMember("on")
    val `Placed.Subjective.on` = Symbol.requiredPackage("loci.embedding.Placed.Subjective").typeMember("on")
    val on = TypeRepr.of[Placement.On[?]].typeSymbol
    val placed = TypeRepr.of[Placed.type].typeSymbol
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]].typeSymbol
    val function1 = TypeRepr.of[Function1[?, ?]].typeSymbol
    val contextResultCount = TypeRepr.of[annotation.internal.ContextResultCount].typeSymbol

  object types:
    val placed = TypeRepr.of[Placed[_, _]]
    val remote = TypeRepr.of[language.Remote[_]]
    val conversion = TypeRepr.of[Conversion[_, _]]

  object names:
    val sbj = "sbj"


  case class PlacementInfo(tpe: TypeRepr, canonical: Boolean, canonicalType: TypeRepr, valueType: TypeRepr, peerType: TypeRepr, subjective: Option[TypeRepr])

  object PlacementInfo:
    def apply(tpe: TypeRepr, acceptUnliftedSubjectiveFunction: Boolean = false): Option[PlacementInfo] =
      def modality(tpe: TypeRepr) = tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.per =>
          Some(PlacementInfo(tpe, canonical = true, tpe, args.head, defn.NothingClass.typeRef, Some(args.last)))
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.subjective =>
          Some(PlacementInfo(tpe, canonical = false, symbols.per.typeRef.appliedTo(args.reverse), args.last, defn.NothingClass.typeRef, Some(args.head)))
        case _ =>
          None

      def placement(tpe: TypeRepr) = tpe match
        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
          Some(modality(args.head).fold(PlacementInfo(tpe, canonical = true, tpe, args.head, args.last, None)) { info =>
            val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, args.last))
            info.copy(tpe = tpe, canonicalType = canonicalType, peerType = args.last)
          })
        case AppliedType(tycon, args)
          if tycon.typeSymbol == symbols.`embedding.on` ||
             tycon.typeSymbol == symbols.`Placed.on` ||
             tycon.typeSymbol == symbols.`Placed.Subjective.on` =>
          Some(modality(args.head).fold(PlacementInfo(tpe, canonical = false, symbols.`language.on`.typeRef.appliedTo(args), args.head, args.last, None)) { info =>
            val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(info.canonicalType, args.last))
            info.copy(tpe = tpe, canonical = false, canonicalType = canonicalType, peerType = args.last)
          })
        case _ =>
          None

      tpe match
        case AppliedType(tycon, List(remote, _))
          if acceptUnliftedSubjectiveFunction && tycon.typeSymbol == symbols.function1 && remote <:< types.remote =>
           placement(tpe) collect { case info if info.subjective.isEmpty =>
             val subjective = remote.widenDealias.typeArgs.head
             val valueType = symbols.per.typeRef.appliedTo(List(info.valueType, subjective))
             val canonicalType = symbols.`language.on`.typeRef.appliedTo(List(valueType, info.peerType))
             info.copy(tpe = tpe, canonical = false, canonicalType = canonicalType, valueType = valueType, subjective = Some(subjective))
           }
        case _ =>
          placement(tpe)
    end apply
  end PlacementInfo


  val optionalSymbolMutator = SymbolMutator.make

  // TODO: pos
  @threadUnsafe lazy val symbolMutator = optionalSymbolMutator getOrElse
    report.errorAndAbort("Placement type inference not supported with current compiler version. Type needs to be ascribed explicitly.")

  def replaceResultType(tpe: TypeRepr, res: TypeRepr): TypeRepr = tpe match
    case MethodType(paramNames, paramTypes, resType) =>
      MethodType(paramNames)(_ => paramTypes, _ => replaceResultType(resType, res))
    case PolyType(paramNames, paramBounds, resType) =>
      MethodType(paramNames)(_ => paramBounds, _ => replaceResultType(resType, res))
    case ByNameType(underlying) =>
      ByNameType(replaceResultType(underlying, res))
    case _ =>
      res

//  def contextResultCount(tpe: TypeRepr): Int =
//    if tpe.isContextFunctionType then contextResultCount(tpe.typeArgs.last) + 1 else 0

  def contextResultCount(symbol: Symbol) =
    symbol.getAnnotation(symbols.contextResultCount) match
      case Some(Apply(_, List(Literal(IntConstant(count))))) => count
      case _ => 0

  def tryIncreasingContextResultCount(symbol: Symbol) =
    optionalSymbolMutator foreach { symbolMutator =>
      symbolMutator.updateAnnotation(
        symbol,
        symbols.contextResultCount,
        List(Literal(IntConstant(contextResultCount(symbol) + 1))))
    }

  class RefChanger(from: Symbol, to: Symbol) extends TreeMap:
    private val toTerm = Ref(to)
    override def transformTerm(term: Term)(owner: Symbol) = term match
      case ref: Ref if ref.symbol == from => toTerm
      case _ => super.transformTerm(term)(owner)

  def normalBody(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) =
    rhs map { rhs =>
      (placementInfo.peerType.asType, symbols.`embedding.on`.typeRef.appliedTo(placementInfo.canonicalType.typeArgs).asType) match
        case ('[p], '[t]) =>
          given Quotes = symbol.asQuotes
          '{ (_: Placement.Context[p]) ?=> ${rhs.asExpr}; erased: t }.asTerm
    }

  def stripPlacedExpressionSyntax(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) =
    normalBody(symbol, placementInfo, rhs map {
      case Apply(Apply(fun, List(Lambda(_, rhs))), _) if fun.symbol.owner == symbols.on => rhs
      case rhs => rhs
    })

  def placedExpressionSyntaxInfo(rhs: Option[Term]) =
    rhs match
      case Some(Apply(Apply(fun, List(Lambda(_, _))), _)) if fun.symbol.owner == symbols.on => (true, fun.symbol.name == names.sbj)
      case _ => (false, false)

  def stripPlacementLiftingConversion(term: Term) =
    def stripPlacementLiftingConversion(term: Term) = term match
      case Inlined(Some(call), List(conversion: ValDef), Block(List(DefDef("body", List(), _, Some(rhs))), dummy: Typed))
        if call.symbol == symbols.placed &&
           conversion.tpt.tpe <:< types.conversion &&
           dummy.tpe <:< types.placed =>
        rhs.underlyingArgument
      case Inlined(Some(call), List(conversion: ValDef, ValDef(_, _, Some(rhs))), Block(List(DefDef("body", List(), _, _)), dummy: Typed))
        if call.symbol == symbols.placed &&
           conversion.tpt.tpe <:< types.conversion &&
           dummy.tpe <:< types.placed =>
        rhs.underlyingArgument
      case rhs =>
        rhs

    term match
      case Block(statements, expr) => Block.copy(term)(statements, stripPlacementLiftingConversion(expr))
      case _ => stripPlacementLiftingConversion(term)
  end stripPlacementLiftingConversion

  def stripConversion2(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) = rhs match
    case Some(Lambda(_, rhs)) => normalBody(symbol, placementInfo, Some(stripPlacementLiftingConversion(rhs)))
    case _ => rhs

  def stripConversion3(symbol: Symbol, placementInfo: PlacementInfo, rhs: Option[Term]) = rhs match
    case Some(Lambda(_, Lambda(List(arg), rhs))) =>
      val MethodType(paramNames, paramTypes, _) = arg.symbol.owner.info: @unchecked
      val tpe = MethodType(paramNames)(_ => paramTypes, _ => placementInfo.valueType)
      val lambda = Lambda(arg.symbol.owner.owner, tpe, { (symbol, args) =>
        RefChanger(arg.symbol, args.head.symbol).transformTree(stripPlacementLiftingConversion(rhs).changeOwner(symbol))(symbol)
      })
      normalBody(symbol, placementInfo, Some(lambda))
    case _ =>
      rhs

  val body = module.body map {
    case stat: ValDef =>
      val symbol = stat.symbol
      val info = symbol.info
      val (placedExpressionSyntax, sbjPlacedExpressionSyntax) = placedExpressionSyntaxInfo(stat.rhs)
      val placementInfo = PlacementInfo(info.resultType, acceptUnliftedSubjectiveFunction = sbjPlacedExpressionSyntax /* && <type NOT ascribed> */)

      if placementInfo.isEmpty && placedExpressionSyntax then
        report.errorAndAbort("Placement expression for value with no placement type", stat.pos)

      placementInfo.fold(stat) { placementInfo =>
        if !placementInfo.canonical then
//          if <type ascribed> then
//            report.errorAndAbort("...", stat.pos)
          symbolMutator.setInfo(symbol, replaceResultType(info, placementInfo.canonicalType))

        val rhs =
          if placedExpressionSyntax then
            stripPlacedExpressionSyntax(symbol, placementInfo, stat.rhs)
            //TODO tryIncreasingContextResultCount(symbol)
          else if placementInfo.subjective.isEmpty then
            stripConversion2(symbol, placementInfo, stat.rhs)
          else
            stripConversion3(symbol, placementInfo, stat.rhs)
            //TODO tryIncreasingContextResultCount(symbol) (?)

//        println(placementInfo.tpe.show + " ~> " + placementInfo.canonicalType.show)
//        println(placementInfo.canonical)
//        println(rhs.map(_.show))

        ValDef.copy(stat)(stat.name, TypeTree.of(using placementInfo.canonicalType.asType), rhs)
      }
//      val symbol = stat.symbol
//      symbol.info match
//        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
//          ValDef.copy(stat)(name, tpt, stripConversion(symbol, args.last, args.head, rhs))
//        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`embedding.on` =>
//          val info = AppliedType(symbols.`language.on`.typeRef, args)
//          symbolMutator.setInfo(symbol, info)
//          ValDef.copy(stat)(name, TypeTree.of(using info.asType), stripPlacedExpressionSyntax(symbol, args.last, args.head, rhs))
//        case _ =>
//          // TODO: check error
//          stat
//    case stat @ DefDef(name, paramss, tpt, rhs) =>
//      val symbol = stat.symbol
//      symbol.info.resultType match
//        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`language.on` =>
//          DefDef.copy(stat)(name, paramss, tpt, stripConversion(symbol, args.last, args.head, rhs))
//        case AppliedType(tycon, args) if tycon.typeSymbol == symbols.`embedding.on` =>
//          val info = AppliedType(symbols.`language.on`.typeRef, args)
//          tryIncreasingContextResultCount(symbol)
//          symbolMutator.setInfo(symbol, replaceResultType(symbol.info, info))
//          DefDef.copy(stat)(name, paramss, TypeTree.of(using info.asType), stripPlacedExpressionSyntax(symbol, args.last, args.head, rhs))
//        case _ =>
//          // TODO: check error
//          stat
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
