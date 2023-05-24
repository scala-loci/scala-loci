package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable

@experimental
trait Synthesis:
  this: Component & Commons & ErrorReporter & Annotations & PlacementInfo & PeerInfo =>
  import quotes.reflect.*

  sealed trait SynthesizedDefinition

  object SynthesizedDefinition:
//    case class Split(original: Symbol, proxy: Symbol, proxyInit: Symbol, placed: Symbol, placedInit: Symbol) extends SynthesizedDefinition
//    case class Split(original: Symbol, universal: Symbol, universalProxy: Option[Symbol], universalInit: Symbol, placedInit: Symbol) extends SynthesizedDefinition
    case class Split(original: Symbol, universal: Symbol, universalInit: Symbol, placedInit: Symbol) extends SynthesizedDefinition
    case class Moved(original: Symbol, placed: Symbol) extends SynthesizedDefinition
    case object None extends SynthesizedDefinition

  def mangleSymbolName(symbol: Symbol) = f"loci$$${s"${implementationForm(symbol)} ${fullName(symbol)}".hashCode}%08x"

  private val synthesizedMovedSplitCache = mutable.Map.empty[Symbol, SynthesizedDefinition.Moved | SynthesizedDefinition.Split]
  private val synthesizedMovedCache = mutable.Map.empty[Symbol, SynthesizedDefinition.Moved]
  private val placedValuesSymbolCache = mutable.Map.empty[(Symbol, Symbol), Symbol]

  private def implementationForm(symbol: Symbol) =
    if symbol.flags is Flags.Module then "object"
    else if symbol.flags is Flags.Trait then "trait"
    else "class"

  private def syntheticTrait(owner: Symbol, name: String, mangledName: String, parents: List[TypeRepr])(decls: Symbol => List[Symbol]) =
    val symbol = owner.typeMember(name)
    if !symbol.exists then
      val symbol = owner.typeMember(mangledName)
      if !symbol.exists then
        val symbol = Symbol.newClass(owner, if canMakeTargetName then name else mangledName, parents, decls, selfType = None)
        SymbolMutator.getOrErrorAndAbort.setFlag(symbol, Flags.Trait | Flags.Synthetic)
        tryMakeTargetName(symbol, mangledName)
        symbol
      else
        symbol
    else
      symbol

  private def copyAnnotations(from: Symbol, to: Symbol, decrementContextResultCount: Boolean) =
    if from.annotations.nonEmpty then
      SymbolMutator.get.fold(
        report.warning("Annotations in multitier modules are ignored with current compiler version.", from.annotations.head.posInUserCode)):
        symbolMutator =>
          from.annotations foreach:
            case tree @ Apply(fun, List(arg @ Literal(IntConstant(count))))
              if decrementContextResultCount &&
                 fun.symbol.isClassConstructor &&
                 fun.symbol.owner == symbols.contextResultCount =>
              if count > 1 then
                symbolMutator.updateAnnotationWithTree(to, Apply.copy(tree)(fun, List(Literal.copy(arg)(IntConstant(count - 1)))))
            case tree =>
              symbolMutator.updateAnnotationWithTree(to, tree)

  private def erasePlacementType(info: TypeRepr) =
    PlacementInfo(info.resultType).fold(info -> defn.AnyClass) { placementInfo =>
      val erasedInfo = placementInfo.modality match
        case Modality.Subjective(peerType) =>
          info.withResultType(symbols.function1.typeRef.appliedTo(List(symbols.remote.typeRef.appliedTo(peerType), placementInfo.valueType)))
        case _ =>
          info.withResultType(placementInfo.valueType)
      erasedInfo -> placementInfo.peerType.typeSymbol
    }

  def synthesizedVal(symbol: Symbol): SynthesizedDefinition.Moved | SynthesizedDefinition.Split = synthesizedMovedSplitCache.getOrElse(symbol, {
    val (info, peer) = erasePlacementType(symbol.info)

    if peer != defn.AnyClass && !(symbol.flags is Flags.Lazy) && !(symbol.flags is Flags.Deferred) && !(symbol.flags is Flags.Inline) then
      val universalName = if symbol.flags is Flags.Private then s"<placed private ${symbol.name} of ${fullName(symbol.owner)}>" else symbol.name
      val placedName = s"<placed ${symbol.name} of ${fullName(symbol.owner)}>"
      val universalValues = placedValuesSymbol(symbol.owner, defn.AnyClass)
      val placedValues = placedValuesSymbol(symbol.owner, peer)

      synthesizedMovedSplitCache.getOrElse(symbol, {
        val universal =
          val universal = universalValues.fieldMember(universalName)
          if !universal.exists then
            val universal = Symbol.newVal(universalValues, universalName, info, symbol.flags &~ Flags.PrivateLocal, Symbol.noSymbol)
            copyAnnotations(symbol, universal, decrementContextResultCount = false)
            universal
          else
            universal

//        val universalProxy = Option.when(symbol.flags is Flags.Private):
//          val universalProxy = universalValues.fieldMember(proxyName)
//          if !universalProxy.exists then
//            Symbol.newMethod(universalValues, proxyName, ByNameType(info), Flags.Synthetic, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
//          else
//            universalProxy

        val universalInit =
          val universalInit = universalValues.fieldMember(placedName)
          if !universalInit.exists then
            Symbol.newMethod(universalValues, placedName, MethodType(List.empty)(_ => List.empty, _ => info), Flags.Synthetic, Symbol.noSymbol)
          else
            universalInit

        val placedInit =
          val placedInit = placedValues.fieldMember(placedName)
          if !placedInit.exists then
            val placedInit = Symbol.newMethod(placedValues, placedName, MethodType(List.empty)(_ => List.empty, _ => info), Flags.Synthetic | Flags.Override, Symbol.noSymbol)
            copyAnnotations(symbol, placedInit, decrementContextResultCount = false)
            placedInit
          else
            placedInit

        val definition = SynthesizedDefinition.Split(symbol, universal, universalInit, placedInit)
        synthesizedMovedSplitCache += symbol -> definition
        synthesizedMovedSplitCache += universal -> definition
//        universalProxy foreach { synthesizedMovedSplitCache += _ -> definition }
        synthesizedMovedSplitCache += universalInit -> definition
        synthesizedMovedSplitCache += placedInit -> definition
        definition
      })
    else
      synthesizedValOrDef(symbol, isVal = true)
  })

  def synthesizedDef(symbol: Symbol) =
    synthesizedValOrDef(symbol, isVal = false)

  private def synthesizedValOrDef(symbol: Symbol, isVal: Boolean): SynthesizedDefinition.Moved = synthesizedMovedCache.getOrElse(symbol, {
    val (name, info, peer) =
      symbol.info match
        case MethodType(List(name), List(param), result)
          if result.typeSymbol == defn.UnitClass &&
             !isVal &&
             symbol.isFieldAccessor &&
             (symbol.name endsWith "_=") =>
          val name =
            if symbol.flags is Flags.Private then
              s"<placed private ${symbol.name.dropRight(2)} of ${fullName(symbol.owner)}>_="
            else
              symbol.name
          val (info, _) = erasePlacementType(param)
          (name, MethodType(List(name))(_ => List(info), _ => result), defn.AnyClass)
        case _ =>
          val (info, peer) = erasePlacementType(symbol.info)
          (symbol.name, info, peer)

    val placedValues = placedValuesSymbol(symbol.owner, peer)

    synthesizedMovedCache.getOrElse(symbol, {
      val placed =
        placedValues.methodMember(name) find { _.info =:= info } getOrElse {
          val placed =
            if isVal then
              Symbol.newVal(placedValues, name, info, symbol.flags | Flags.Synthetic, Symbol.noSymbol)
            else
              Symbol.newMethod(placedValues, name, info, symbol.flags | Flags.Synthetic, Symbol.noSymbol)
          copyAnnotations(symbol, placed, decrementContextResultCount = info != symbol.info)
          placed
        }
      val definition = SynthesizedDefinition.Moved(symbol, placed)
      synthesizedMovedCache += symbol -> definition
      synthesizedMovedCache += placed -> definition
      synthesizedMovedSplitCache += symbol -> definition
      synthesizedMovedSplitCache += placed -> definition
      definition
    })
  })

  def synthesizedDefinition(symbol: Symbol): SynthesizedDefinition =
    if symbol.isField then
      synthesizedVal(symbol)
    else if symbol.isMethod then
      synthesizedDef(symbol)
    else
      SynthesizedDefinition.None

  def placedValuesSymbol(module: Symbol, peer: Symbol): Symbol = placedValuesSymbolCache.getOrElse(module -> peer, {
    val name = fullName(module)
    val mangledName = mangleSymbolName(module)
    val form = implementationForm(module)
    val separator = if module.isType && !module.isPackageDef && !module.isModuleDef then "#" else "."
    val parents = List(TypeRepr.of[Object], if peer == defn.AnyClass then types.placedValues else placedValuesSymbol(module, defn.AnyClass).typeRef)
    syntheticTrait(
      module,
      if peer == defn.AnyClass then s"<placed values of $form $name>" else s"<placed values on $name$separator${peer.name}>",
      if peer == defn.AnyClass then mangledName else s"$mangledName$$${peer.name}",
      parents): symbol =>
        placedValuesSymbolCache += module -> peer -> symbol
        module.declarations flatMap { decl =>
          synthesizedDefinition(decl) match
//            case SynthesizedDefinition.Split(_, universal, universalProxy, universalInit, _) if peer == defn.AnyClass =>
//              universalProxy.fold(List(universal, universalInit)) { List(universal, _, universalInit) }
            case SynthesizedDefinition.Split(_, universal, universalInit, _) if peer == defn.AnyClass =>
              List(universal, universalInit)
            case SynthesizedDefinition.Split(_, _, _, placedInit) if placedInit.owner == symbol =>
              List(placedInit)
            case SynthesizedDefinition.Moved(_, placed) if placed.owner == symbol =>
              List(placed)
            case _ =>
              List.empty
        }
  })
end Synthesis
