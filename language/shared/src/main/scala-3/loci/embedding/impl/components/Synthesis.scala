package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.collection.mutable

trait Synthesis:
  this: Component with Commons with ErrorReporter with Annotations with PlacementInfo with PeerInfo =>
  import quotes.reflect.*

  private val placedValuesSymbolCache = mutable.Map.empty[Symbol, Symbol]
  private val peerCommonSymbolCache = mutable.Map.empty[Symbol, Symbol]
  private val peerSymbolCache = mutable.Map.empty[Symbol, Symbol]

  private def mangle(name: String) = f"loci$$${name.hashCode}%08x"

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

  private def copyAnnotations(from: Symbol, to: Symbol) =
    if from.annotations.nonEmpty then
      SymbolMutator.get.fold(
        report.warning("Annotations in multitier modules are ignored with current compiler version.", from.annotations.head.posInUserCode)):
        symbolMutator => from.annotations foreach { symbolMutator.updateAnnotationWithTree(to, _) }

  private def erasePlacementType(info: TypeRepr) =
    PlacementInfo(info.resultType).fold(info) { placementInfo =>
      if placementInfo.modality.subjective then
        info.withResultType(TypeRepr.of[Unit])
      else
        info.withResultType(placementInfo.valueType)
    }

  def placedValuesSymbol(module: Symbol): Symbol = placedValuesSymbolCache.getOrElse(module, {
    val name = fullName(module)
    val form = implementationForm(module)
    val parents = List(TypeRepr.of[Object], types.placedValues)
    syntheticTrait(module, s"<placed values of $form $name>", mangle(s"$form $name"), parents): symbol =>
      placedValuesSymbolCache += module -> symbol
      peerCommonSymbol(module) :: (PeerInfo.ofModule(module) map { peerInfo => peerSymbol(peerInfo.peerType.typeSymbol) })
  })

  def peerCommonSymbol(module: Symbol): Symbol = peerCommonSymbolCache.getOrElse(module, {
    val name = fullName(module)
    val form = implementationForm(module)
    val parents = List(TypeRepr.of[Object])
    syntheticTrait(placedValuesSymbol(module), s"<peer of $form $name>", s"${mangle(s"$form $name")}$$peer", parents): symbol =>
      peerCommonSymbolCache += module -> symbol
      module.declarations collect {
        case decl if decl.isField =>
          val privateWithin = if decl.flags is Flags.Protected then decl.protectedWithin else decl.privateWithin
          val field = Symbol.newVal(symbol, decl.name, erasePlacementType(decl.info), decl.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })
          copyAnnotations(decl, field)
          field
        case decl if decl.isMethod =>
          val privateWithin = if decl.flags is Flags.Protected then decl.protectedWithin else decl.privateWithin
          val method = Symbol.newMethod(symbol, decl.name, erasePlacementType(decl.info), decl.flags, privateWithin.fold(Symbol.noSymbol) { _.typeSymbol })

          // TODO: how to deal with "ContextResultCount"?

          copyAnnotations(decl, method)
          method
      }
  })

  def peerSymbol(peer: Symbol): Symbol = peerSymbolCache.getOrElse(peer, {
    val module = peer.owner
    val name = fullName(module)
    val form = implementationForm(module)
    val parents = List(TypeRepr.of[Object])
    syntheticTrait(placedValuesSymbol(module), s"<peer ${fullName(peer)}>", s"${mangle(s"$form $name")}$$peer$$${peer.name}", parents): symbol =>
      peerSymbolCache += module -> symbol
      List.empty
  })
end Synthesis
