package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable
import scala.util.control.NonFatal

object Synthesis:
  private val synthesizedDefinitionsCache = mutable.Map.empty[Any, Any]
  private val synthesizedStatementsCache = mutable.Map.empty[Any, Any]
  private val placedValuesSymbolCache = mutable.Map.empty[Any, Any]

@experimental
trait Synthesis:
  this: Component & Commons & Annotations & Placements =>
  import quotes.reflect.*

  case class SynthesizedDefinitions(original: Symbol, binding: Symbol, init: Option[Symbol], impls: List[Symbol])
  case class SynthesizedStatements(binding: Symbol, impls: List[Symbol])

  private val synthesizedDefinitionsCache = Synthesis.synthesizedDefinitionsCache match
    case cache: mutable.Map[Symbol, SynthesizedDefinitions] @unchecked => cache
  private val synthesizedStatementsCache = Synthesis.synthesizedStatementsCache match
    case cache: mutable.Map[(Symbol, Symbol, Int), Option[SynthesizedStatements]] @unchecked => cache
  private val placedValuesSymbolCache = Synthesis.placedValuesSymbolCache match
    case cache: mutable.Map[(Symbol, Symbol), Symbol] @unchecked => cache

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
    def updateSymbolAnnotationWithTree(symbol: Symbol, tree: Tree): Unit =
      SymbolMutator.get.fold(
        report.warning("Annotations in multitier modules are ignored with current compiler version.", from.annotations.head.posInUserCode)):
        _.updateAnnotationWithTree(symbol, tree)

    from.annotations foreach:
      case tree @ Apply(fun, List(arg @ Literal(IntConstant(count))))
        if decrementContextResultCount &&
           fun.symbol.isClassConstructor &&
           fun.symbol.owner == symbols.contextResultCount =>
        if count > 1 then
          updateSymbolAnnotationWithTree(to, Apply.copy(tree)(fun, List(Literal.copy(arg)(IntConstant(count - 1)))))
      case tree =>
        updateSymbolAnnotationWithTree(to, tree)

  private def erasePlacementType(info: TypeRepr) =
    PlacementInfo(info.resultType).fold(info -> defn.AnyClass): placementInfo =>
      val erasedInfo = placementInfo.modality match
        case Modality.Subjective(peerType) =>
          info.withResultType(symbols.function1.typeRef.appliedTo(List(symbols.remote.typeRef.appliedTo(peerType), placementInfo.valueType)))
        case _ =>
          info.withResultType(placementInfo.valueType)
      erasedInfo -> placementInfo.peerType.typeSymbol

  private def synthesizedValOrDef(symbol: Symbol): SynthesizedDefinitions = synthesizedDefinitionsCache.getOrElse(symbol, {
    val placedName = s"<placed ${symbol.name} of ${fullName(symbol.owner)}>"
    val (universalName, info, peer) =
      symbol.info match
        case MethodType(List(paramName), List(paramType), resultType)
          if resultType.typeSymbol == defn.UnitClass &&
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
          val name = if symbol.flags is Flags.Private then s"<placed private ${symbol.name} of ${fullName(symbol.owner)}>" else symbol.name
          val (info, peer) = erasePlacementType(symbol.info)
          (name, info, peer)

    val universalValues = placedValuesSymbol(symbol.owner, defn.AnyClass)
    val placedValues = placedValuesSymbol(symbol.owner, peer)

    synthesizedDefinitionsCache.getOrElse(symbol, {
      val universalOnly = peer == defn.AnyClass || (symbol.flags is Flags.Deferred)
      val flags = if universalOnly then symbol.flags else symbol.flags &~ Flags.PrivateLocal
      val decrementContextResultCount = info != symbol.info

      val universal =
        val universal = universalValues.fieldMember(universalName)
        if !universal.exists then
          val universal = newVal(universalValues, universalName, info, flags, Symbol.noSymbol)
          copyAnnotations(symbol, universal, decrementContextResultCount)
          universal
        else
          universal

      val definition =
        if !universalOnly then
          if symbol.isMethod then
            val placed = placedValues.methodMember(universalName) find { _.info =:= info } getOrElse:
              val placed = newMethod(placedValues, universalName, info, flags | Flags.Synthetic | Flags.Override, Symbol.noSymbol)
              copyAnnotations(symbol, placed, decrementContextResultCount)
              placed
            SynthesizedDefinitions(symbol, universal, None, List(placed))
          else
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
                copyAnnotations(symbol, placedInit, decrementContextResultCount)
                placedInit
              else
                placedInit

            SynthesizedDefinitions(symbol, universal, Some(universalInit), List(placedInit))
        else
          SynthesizedDefinitions(symbol, universal, None, List.empty)
      end definition

      synthesizedDefinitionsCache += symbol -> definition
      definition
    })
  })

  private def synthesizedModule(symbol: Symbol): SynthesizedDefinitions = synthesizedDefinitionsCache.getOrElse(symbol, {
    val module = if symbol.isClassDef then symbol else symbol.moduleClass
    val modulePlacedValues = placedValuesSymbol(module, defn.AnyClass)
    val ownerPlacedValues = placedValuesSymbol(module.owner, defn.AnyClass)
    synthesizedDefinitionsCache.getOrElse(symbol, {
      val binding =
        val binding = ownerPlacedValues.fieldMember(module.companionModule.name)
        if !binding.exists then
          newVal(ownerPlacedValues, module.companionModule.name, modulePlacedValues.typeRef, Flags.Final | Flags.Lazy | Flags.StableRealizable, Symbol.noSymbol)
        else
          binding
      val definition = SynthesizedDefinitions(module, binding, None, List.empty)
      synthesizedDefinitionsCache += symbol -> definition
      definition
    })
  })

  def synthesizedDefinitions(symbol: Symbol): SynthesizedDefinitions =
    if symbol.isModuleDef then
      synthesizedModule(symbol)
    else
      synthesizedValOrDef(symbol)

  def synthesizedStatement(module: Symbol, peer: Symbol, index: Int): Option[SynthesizedStatements] =
    synthesizedStatementsCache.getOrElse((module, peer, index), {
      if peer != defn.AnyClass then
        val name = s"<placed statement ${index} of ${fullName(peer)}>"
        val universalValues = placedValuesSymbol(module, defn.AnyClass)
        val placedValues = placedValuesSymbol(module, peer)
        val unaryProcedureType = MethodType(List.empty)(_ => List.empty, _ => TypeRepr.of[Unit])

        synthesizedStatementsCache.getOrElse((module, peer, index), {
          val binding =
            val binding = universalValues.fieldMember(name)
            if !binding.exists then
              newMethod(universalValues, name, unaryProcedureType, Flags.Synthetic, Symbol.noSymbol)
            else
              binding

          val impl =
            val impl = placedValues.fieldMember(name)
            if !impl.exists then
              newMethod(placedValues, name, unaryProcedureType, Flags.Synthetic | Flags.Override, Symbol.noSymbol)
            else
              impl

          val statement = Some(SynthesizedStatements(binding, List(impl)))
          synthesizedStatementsCache += (module, peer, index) -> statement
          statement
        })
      else
        synthesizedStatementsCache += (module, peer, index) -> None
        None
    })

  def placedValuesSymbol(module: Symbol, peer: Symbol): Symbol = placedValuesSymbolCache.getOrElse((module, peer), {
    val name = fullName(module)
    val mangledName = mangleSymbolName(module)
    val form = implementationForm(module)
    val separator = if module.isType && !module.isPackageDef && !module.isModuleDef then "#" else "."
    val parents =
      TypeRepr.of[Object] :: (
        if !isMultitierModule(module) then List.empty
        else if peer == defn.AnyClass then List(types.placedValues)
        else List(placedValuesSymbol(module, defn.AnyClass).typeRef))

    val symbol = syntheticTrait(
      module,
      if peer == defn.AnyClass then s"<placed values of $form $name>" else s"<placed values on $name$separator${peer.name}>",
      if peer == defn.AnyClass then mangledName else s"$mangledName$$${peer.name}",
      parents): symbol =>
        placedValuesSymbolCache += (module, peer) -> symbol

        def collectDeclarations(impls: List[Symbol]) =
          impls collect { case impl if impl.owner == symbol => impl }

        val indices = mutable.Map.empty[Symbol, Int]
        val declarations =
          try module.tree match
            case ClassDef(_, _, _, _, body) =>
              body flatMap:
                case stat @ (_: ValDef | _: DefDef | _: ClassDef)
                    if (stat.symbol.isField || stat.symbol.isMethod) && !stat.symbol.isModuleDef ||
                       stat.symbol.isClassDef && stat.symbol.isModuleDef=>
                  val definitions = synthesizedDefinitions(stat.symbol)
                  collectDeclarations(definitions.binding :: definitions.init.toList ++ definitions.impls)
                case statement: Term =>
                  val statementPeer = PlacementInfo(statement.tpe.resultType).fold(defn.AnyClass) { _.peerType.typeSymbol }
                  if peer == defn.AnyClass || peer == statementPeer then
                    val index = indices.getOrElse(statementPeer, 0)
                    indices += statementPeer -> (index + 1)
                    synthesizedStatement(module, statementPeer, index).toList flatMap: statement =>
                      collectDeclarations(statement.binding :: statement.impls)
                  else
                    List.empty
                case _ =>
                  List.empty
            case _ =>
              List.empty
          catch
            case NonFatal(_) =>
              List.empty

        val decls =
          if declarations.isEmpty then
            module.declarations flatMap: decl =>
              if (decl.isField || decl.isMethod) && !decl.isModuleDef || decl.isClassDef && decl.isModuleDef then
                val definitions = synthesizedDefinitions(decl)
                collectDeclarations(definitions.binding :: definitions.init.toList ++ definitions.impls)
              else
                List.empty
          else
            declarations

        if module.owner hasAncestor isMultitierModule then
          val name = s"<outer placed values of ${implementationForm(module.owner)} ${fullName(module.owner)}>"
          val tpe = placedValuesSymbol(module.owner, defn.AnyClass).typeRef
          newVal(symbol, name, tpe, Flags.ParamAccessor, Symbol.noSymbol) :: decls
        else
          decls
    end symbol

    val (names, tpes) = (symbol.declaredFields collect { case symbol if symbol.isParamAccessor => symbol.name -> symbol.info }).unzip
    val tpe = MethodType(names)(_ => tpes, _ => symbol.typeRef)
    SymbolMutator.getOrErrorAndAbort.setInfo(symbol.primaryConstructor, tpe)

    symbol
  })
end Synthesis
