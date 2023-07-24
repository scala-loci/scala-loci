package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable

@experimental
trait Synthesis:
  this: Component & Commons & Annotations & Placements =>
  import quotes.reflect.*

  case class SynthesizedDefinitions(original: Symbol, binding: Option[Symbol], impls: List[Symbol])

  private val synthesizedDefinitionsCache = mutable.Map.empty[Symbol, SynthesizedDefinitions]
  private val placedValuesSymbolCache = mutable.Map.empty[(Symbol, Symbol), Symbol]

  private def mangleSymbolName(symbol: Symbol) = f"loci$$${s"${implementationForm(symbol)} ${fullName(symbol)}".hashCode}%08x"

  private def implementationForm(symbol: Symbol) =
    if symbol.flags is Flags.Module then "object"
    else if symbol.flags is Flags.Trait then "trait"
    else "class"

  private def syntheticTrait(owner: Symbol, name: String, mangledName: String, parents: List[TypeRepr])(decls: Symbol => List[Symbol]) =
    val symbol = owner.typeMember(name)
    if !symbol.exists then
      val symbol = owner.typeMember(mangledName)
      if !symbol.exists then
        val symbol = newClass(owner, if canMakeTargetName then name else mangledName, Flags.Synthetic | Flags.Trait, parents, decls, selfType = None)
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
    PlacementInfo(info.resultType).fold(info -> defn.AnyClass): placementInfo =>
      val erasedInfo = placementInfo.modality match
        case Modality.Subjective(peerType) =>
          info.withResultType(symbols.function1.typeRef.appliedTo(List(symbols.remote.typeRef.appliedTo(peerType), placementInfo.valueType)))
        case _ =>
          info.withResultType(placementInfo.valueType)
      erasedInfo -> placementInfo.peerType.typeSymbol

  def synthesizedVal(symbol: Symbol): SynthesizedDefinitions = synthesizedDefinitionsCache.getOrElse(symbol, {
    val (info, peer) = erasePlacementType(symbol.info)

    if peer != defn.AnyClass && !(symbol.flags is Flags.Lazy) && !(symbol.flags is Flags.Deferred) && !(symbol.flags is Flags.Inline) then
      val universalName = if symbol.flags is Flags.Private then s"<placed private ${symbol.name} of ${fullName(symbol.owner)}>" else symbol.name
      val placedName = s"<placed ${symbol.name} of ${fullName(symbol.owner)}>"
      val universalValues = placedValuesSymbol(symbol.owner, defn.AnyClass)
      val placedValues = placedValuesSymbol(symbol.owner, peer)

      synthesizedDefinitionsCache.getOrElse(symbol, {
        val universal =
          val universal = universalValues.fieldMember(universalName)
          if !universal.exists then
            val universal = newVal(universalValues, universalName, info, symbol.flags &~ Flags.PrivateLocal, Symbol.noSymbol)
            copyAnnotations(symbol, universal, decrementContextResultCount = false)
            universal
          else
            universal

//        val universalProxy = Option.when(symbol.flags is Flags.Private):
//          val universalProxy = universalValues.fieldMember(proxyName)
//          if !universalProxy.exists then
//            newMethod(universalValues, proxyName, ByNameType(info), Flags.Synthetic, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
//          else
//            universalProxy

        val universalInit =
          val universalInit = universalValues.fieldMember(placedName)
          if !universalInit.exists then
            newMethod(universalValues, placedName, MethodType(List.empty)(_ => List.empty, _ => info), Flags.Synthetic, Symbol.noSymbol)
          else
            universalInit

        val placedInit =
          val placedInit = placedValues.fieldMember(placedName)
          if !placedInit.exists then
            val placedInit = newMethod(placedValues, placedName, MethodType(List.empty)(_ => List.empty, _ => info), Flags.Synthetic | Flags.Override, Symbol.noSymbol)
            copyAnnotations(symbol, placedInit, decrementContextResultCount = false)
            placedInit
          else
            placedInit

        val definition = SynthesizedDefinitions(symbol, Some(universal), List(universalInit, placedInit))
        synthesizedDefinitionsCache += symbol -> definition
        definition
      })
    else
      synthesizedValOrDef(symbol, isVal = true)
  })

  def synthesizedDef(symbol: Symbol): SynthesizedDefinitions =
    synthesizedValOrDef(symbol, isVal = false)

  private def synthesizedValOrDef(symbol: Symbol, isVal: Boolean): SynthesizedDefinitions = synthesizedDefinitionsCache.getOrElse(symbol, {
    val (name, info, peer) =
      symbol.info match
        case MethodType(List(paramName), List(paramType), resultType)
          if resultType.typeSymbol == defn.UnitClass &&
             !isVal &&
             symbol.isFieldAccessor &&
             (symbol.name endsWith "_=") =>
          val name =
            if symbol.flags is Flags.Private then
              s"<placed private ${symbol.name.dropRight(2)} of ${fullName(symbol.owner)}>_="
            else
              symbol.name
          val (info, _) = erasePlacementType(paramType)
          (name, MethodType(List(paramName))(_ => List(info), _ => resultType), defn.AnyClass)
        case _ =>
          val (info, peer) = erasePlacementType(symbol.info)
          (symbol.name, info, peer)

    val placedValues = placedValuesSymbol(symbol.owner, peer)

    synthesizedDefinitionsCache.getOrElse(symbol, {
      val impl = placedValues.methodMember(name) find { _.info =:= info } getOrElse:
        val impl =
          if isVal then
            newVal(placedValues, name, info, symbol.flags | Flags.Synthetic, Symbol.noSymbol)
          else
            newMethod(placedValues, name, info, symbol.flags | Flags.Synthetic, Symbol.noSymbol)
        copyAnnotations(symbol, impl, decrementContextResultCount = info != symbol.info)
        impl
      val definition =
        if isVal then
          SynthesizedDefinitions(symbol, Some(impl), List.empty)
        else
          SynthesizedDefinitions(symbol, None, List(impl))
      synthesizedDefinitionsCache += symbol -> definition
      definition
    })
  })

  def synthesizedDefinitions(symbol: Symbol): SynthesizedDefinitions =
    if symbol.isField then
      synthesizedVal(symbol)
    else if symbol.isMethod then
      synthesizedDef(symbol)
    else
      SynthesizedDefinitions(symbol, None, List.empty)

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
        module.declarations flatMap: decl =>
          val definitions = synthesizedDefinitions(decl)
          (definitions.binding collect { case binding if binding.owner == symbol => binding }) ++
          (definitions.impls collect { case impl if impl.owner == symbol => impl })
  })
end Synthesis
