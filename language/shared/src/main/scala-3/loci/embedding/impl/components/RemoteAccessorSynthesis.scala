package loci
package embedding
package impl
package components

import utility.noMacroCheck
import utility.reflectionExtensions.*
import loci.language.AccessorGeneration.*

import java.util.IdentityHashMap
import scala.annotation.experimental
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.SeqMap
import scala.collection.mutable
import scala.util.control.NonFatal

object RemoteAccessorSynthesis:
  private val synthesizedModuleSignatureCache = mutable.Map.empty[Any, Any]
  private val synthesizedPeerSignatureCache = mutable.Map.empty[Any, Any]
  private val synthesizedAccessorsCache = mutable.Map.empty[Any, Any]

@experimental
trait RemoteAccessorSynthesis:
  this: Component & Commons & ErrorReporter & Placements & Peers & Annotations =>
  import quotes.reflect.*

  case class Accessors(
    identifier: (Symbol, Option[ValDef]),
    signature: (Symbol, Option[ValDef]),
    peers: SeqMap[Symbol, (Symbol, Option[ValDef], Symbol, Option[DefDef])],
    marshalling: CachedTypeSeqMap[(Symbol, Option[ValDef])],
    placed: SeqMap[Symbol | Int, (Symbol, Option[ValDef])])

  private val synthesizedModuleSignatureCache = RemoteAccessorSynthesis.synthesizedModuleSignatureCache match
    case cache: mutable.Map[Symbol, (Symbol, Symbol)] @unchecked => cache
  private val synthesizedPeerSignatureCache = RemoteAccessorSynthesis.synthesizedPeerSignatureCache match
    case cache: mutable.Map[(Symbol, Symbol), (Symbol, Symbol)] @unchecked => cache
  private val synthesizedAccessorsCache = RemoteAccessorSynthesis.synthesizedAccessorsCache match
    case cache: mutable.Map[Symbol, Accessors] @unchecked => cache

  def synthesizeModuleSignature(module: Symbol) = synthesizedModuleSignatureCache.getOrElse(module, {
    val hasMultitierParent = module.typeRef.baseClasses.tail exists isMultitierModule
    val flags = Flags.Lazy | (if hasMultitierParent then Flags.Override else Flags.EmptyFlags)
    (newVal(module, "$loci$mod", TypeRepr.of[String], flags, Symbol.noSymbol),
     newVal(module, "$loci$sig", types.moduleSignature, flags, Symbol.noSymbol))
  })

  def synthesizePeerSignature(module: Symbol, peer: Symbol) = synthesizedPeerSignatureCache.getOrElse((module, peer), {
    val overridden = (peer.allOverriddenSymbols map { _.owner }).toSet + peer.owner
    val isOverriddingPeer = module.typeRef.baseClasses.tail exists { overridden contains _ }
    val overridingFlags = if isOverriddingPeer then Flags.Override else Flags.EmptyFlags
    val info = ByNameType(symbols.map.typeRef.appliedTo(List(types.peerSignature, types.peerTie)))
    (newVal(module, s"$$loci$$peer$$sig$$${peer.name}", types.peerSignature, Flags.Lazy | overridingFlags, Symbol.noSymbol),
     newMethod(module, s"$$loci$$peer$$ties$$${peer.name}", info, overridingFlags, Symbol.noSymbol))
  })

  private def encodeName(name: String) = name flatMap:
    case '~' => "$tilde"
    case '=' => "$eq"
    case '<' => "$less"
    case '>' => "$greater"
    case '!' => "$bang"
    case '#' => "$hash"
    case '%' => "$percent"
    case '^' => "$up"
    case '&' => "$amp"
    case '|' => "$bar"
    case '*' => "$times"
    case '/' => "$div"
    case '+' => "$plus"
    case '-' => "$minus"
    case ':' => "$colon"
    case '?' => "$qmark"
    case '@' => "$at"
    case '\\' => "$bslash"
    case c => if !Character.isJavaIdentifierPart(c) then f"$$u${c.toInt}%04X" else c.toString

  private def classFileName(symbol: Symbol) =
    constructFullName(symbol,
      name = symbol => encodeName(targetName(symbol)),
      separator = symbol => if symbol.isPackageDef then "." else "$",
      skip = _.isPackageObject)

  private case class TransmittableTypes(base: TypeRepr, intermediate: TypeRepr, result: TypeRepr, proxy: TypeRepr, transmittables: TypeRepr):
    def typeList = List(base, intermediate, result, proxy, transmittables)
    def asMarshallableTypes = MarshallableTypes(base, result, proxy)
    def show = s"Transmittable[${showType(base)}, ${showType(intermediate)}, ${showType(result)}]"
    def showMore = s"$show { type Proxy = ${showType(proxy)} }"
    private def showType(tpe: TypeRepr) = tpe match
      case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass && hi.typeSymbol == defn.AnyClass => "?"
      case _ => tpe.safeShow(Printer.SafeTypeReprShortCode)

  private object TransmittableTypes:
    def apply(transmittable: TypeRepr): TransmittableTypes =
      val resultType = transmittable.resultType
      val typeArgs =
        if resultType derivesFrom symbols.transmittable then
          resultType.baseType(symbols.transmittable).typeArgs
        else
          List(symbols.base, symbols.intermediate, symbols.result, symbols.proxy, symbols.transmittables) map: member =>
            resultType.resolvedMemberType(member) getOrElse TypeRepr.of[Any] match
              case TypeBounds(low, hi) if low =:= hi => low
              case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass => hi
              case TypeBounds(low, hi) if hi.typeSymbol == defn.AnyClass => low
              case tpe => tpe
      val List(base, intermediate, result, proxy, transmittables) = typeArgs: @unchecked
      TransmittableTypes(base, intermediate, result, proxy, transmittables)

  private case class MarshallableTypes(base: TypeRepr, result: TypeRepr, proxy: TypeRepr):
    def typeList = List(base, result, proxy)

  private object MarshallableTypes:
    def apply(marshallable: TypeRepr): MarshallableTypes =
      val List(base, result, proxy) = marshallable.resultType.baseType(symbols.marshallable).typeArgs: @unchecked
      MarshallableTypes(base, result, proxy)

  private class Transmittable(val tree: Term, val types: TransmittableTypes, val signature: String, var marshallable: Option[ValDef])

  private object Transmittable:
    def apply(tree: Term, types: TransmittableTypes, signature: String): Transmittable = new Transmittable(tree, types, signature, None)
    def apply(tree: Term): Transmittable = new Transmittable(tree, TransmittableTypes(tree.tpe), transmittableSignature(tree), None)

  sealed class CachedTypeSeqMap[+T]:
    protected val map: mutable.Map[TypeRepr, T] @uncheckedVariance = mutable.Map.empty
    protected val list: mutable.ListBuffer[T] @uncheckedVariance = mutable.ListBuffer.empty
    def lookupType(tpe: TypeRepr): Option[T] =
      map.get(tpe) orElse:
        map find { (key, _) => key =:= tpe } map: (_, value) =>
          map += tpe -> value
          value
    def iterator: Iterator[T] =
      list.iterator
    def mapValues[U](f: T => U) =
      flatMapValues { v => Some(f(v)) }
    def flatMapValues[U](f: T => Option[U]) =
      val other = CachedTypeSeqMap[U]
      other.list.sizeHint(list)
      other.map.sizeHint(map.size)
      val mapping = IdentityHashMap[T, Option[U]]
      list foreach: value =>
        val mapped = f(value)
        mapping.put(value, mapped)
        mapped foreach { other.list += _ }
      map foreach: (key, value) =>
        mapping.get(value) foreach: value =>
          other.map += key -> value
      other

  sealed class MutableCachedTypeSeqMap[T] extends CachedTypeSeqMap[T]:
    def addNewTypeEntry(tpe: TypeRepr, value: T) =
      assert(lookupType(tpe).isEmpty)
      map += tpe -> value
      list += value

  private def mangledSymbolName(symbol: Symbol) =
    f"${s"${implementationForm(symbol)} ${fullName(symbol)}".hashCode}%08x"

  private def implementationForm(symbol: Symbol) =
    if symbol.flags is Flags.Module then "object"
    else if symbol.flags is Flags.Trait then "trait"
    else "class"

  private object Tuple extends TupleExtractor(quotes)

  private def accessorSignature(name: String | List[TypeToken], params: List[List[TypeRepr]], result: TypeRepr) =
    val paramsSignature = params flatMap: params =>
      TypeToken.`(` :: ((params flatMap { param => TypeToken.`,` ++ TypeToken.typeSignature(param) }).drop(2) :+ TypeToken.`)`)
    val prefix = name match
      case name: String => List(TypeToken(name))
      case name: List[TypeToken] => name
    val signature =
      prefix ++ paramsSignature ++ TypeToken.`:` ++ TypeToken.typeSignature(result)
    TypeToken.serialize(signature)

  private val unknownSignature = "########"

  private val abstractSignature = "abstract"

  private def transmittableSignature(term: Term) =
    TermToken.serializeTerm(term).toOption.fold(unknownSignature): term =>
      f"${term.hashCode}%08x"

  private def marshallableSignature(symbol: Symbol) =
    // TODO implement
    unknownSignature

  private object PlacedBlock:
    def unapply(term: Term) = term match
      case Apply(Apply(invocation @ TypeApply(Select(prefix, _), _ :: value :: _), List(lambda @ Lambda(List(_), block))), _)
          if term.symbol.maybeOwner == symbols.block && lambda.tpe.isContextFunctionType =>
        val captures =
          if prefix.symbol.maybeOwner == symbols.capture then
            prefix match
              case Apply(_, List(Typed(Repeated(captures, _), _))) if prefix.symbol.maybeOwner == symbols.capture => captures
              case Apply(_, captures) if prefix.symbol.maybeOwner == symbols.capture => captures
              case _ => List.empty
          else
            List.empty
        Some(value.tpe, captures map { _.tpe.widenTermRefByName }, invocation.posInUserCode)
      case _ =>
        None

  private object PlacedBlockInvocation:
    def unapply(term: Term) = term match
      case PlacedAccess(_, PlacedBlock(value, captures, pos), _, _, _, _, _) => Some(value, captures, pos)
      case PlacedBlock(_, captures, pos) => Some(TypeRepr.of[Unit], captures, pos)
      case _ => None

  private object Resolution:
    enum Result:
      case Success(term: Term)
      case Failure(message: String)
      case FailureOnTypeParameter(message: String, term: Term)

    object Result:
      def apply(tree: Term) =
        resolutionFailureCollector.foldTree(None, tree)(Symbol.noSymbol) match
          case Some(message, false) => Failure(message)
          case Some(message, true) => FailureOnTypeParameter(message, tree)
          case _ => Success(tree)

      extension (self: Result)
        def asTransmittable(allowFailureForTypeParameters: Boolean) = self match
          case Success(term) => Right(Transmittable(term))
          case Failure(message) => Left(message)
          case FailureOnTypeParameter(message, term) =>
            Either.cond(allowFailureForTypeParameters,
              Transmittable(term, TransmittableTypes(term.tpe), abstractSignature),
              message)

        def asTerm = self match
          case Success(term) => Right(term)
          case Failure(message) => Left(message)
          case FailureOnTypeParameter(message, _) => Left(message)
    end Result

    private object resolutionFailureCollector extends TreeAccumulator[Option[(String, Boolean)]]:
      def foldTree(failure: Option[(String, Boolean)], tree: Tree)(owner: Symbol) = tree match
        case Block(List(defintion @ DefDef(names.resolutionFailure, _, _, _), Apply(call, List())), expr) if defintion.symbol == call.symbol =>
          compileTimeOnly(defintion.symbol).fold(foldOverTree(failure, tree)(owner)): message =>
            val typeParam = TransmittableTypes(expr.tpe).base.typeSymbol.isTypeParam
            failure match
              case Some(_, false) => failure
              case Some(_, true) if typeParam => foldOverTree(failure, tree)(owner)
              case _ if typeParam => foldOverTree(Some(message, typeParam), tree)(owner)
              case _ => Some(message, typeParam)
        case _ =>
          foldOverTree(failure, tree)(owner)

    def resolve(tpe: TypeRepr, message: String) =
      noMacroCheck(Implicits.search(tpe)) match
        case result: ImplicitSearchSuccess => Result(result.tree)
        case _ => Result.Failure(message)

    def resolveSerializable(tpe: TypeRepr) =
      resolve(
        symbols.serializable.typeRef.appliedTo(tpe),
        s"${tpe.safeShow(Printer.SafeTypeReprShortCode)} is not serializable").asTerm

    def resolveTransmittable(tpe: TypeRepr, allowFailureForTypeParameters: Boolean) =
      resolve(
        symbols.transmittable.typeRef.appliedTo(List(tpe, TypeBounds.empty, TypeBounds.empty, TypeBounds.empty, TypeBounds.empty)),
        s"${tpe.safeShow(Printer.SafeTypeReprShortCode)} is not transmittable").asTransmittable(allowFailureForTypeParameters)
  end Resolution

  private def signatures(module: Symbol) =
    extension (tpe: TypeRepr)
      def asTerm: Option[Term] = tpe match
        case AnnotatedType(underlying, _) => underlying.asTerm
        case Refinement(parent, _, _) => parent.asTerm
        case ThisType(tref) => Some(This(tref.typeSymbol))
        case TermRef(NoPrefix(), name) => Some(Ref(tpe.termSymbol))
        case TermRef(qualifier, name) => qualifier.asTerm map { Select.unique(_, name) }
        case _ => None

      def pathTerm: Option[Term] = tpe match
        case tpe: AnnotatedType => tpe.underlying.pathTerm
        case tpe: Refinement => tpe.parent.pathTerm
        case tpe: NamedType => tpe.qualifier.asTerm
        case _ => None
    end extension

    def signature(peerType: TypeRepr, pos: Position) =
      peerType.pathTerm match
        case Some(term) if term.symbol.isModuleDef && isMultitierModule(term.symbol) =>
          val(symbol, _) = synthesizePeerSignature(term.symbol.moduleClass, peerType.typeSymbol)
          Some(term.select(symbol))
        case _ =>
          errorAndCancel(s"Invalid prefix for peer type: ${peerType.safeShow(Printer.SafeTypeReprCode)}", pos)
          None

    val moduleIdentifier =
      val (symbol, _) = synthesizeModuleSignature(module)
      ValDef(symbol, Some(Literal(StringConstant(fullName(module)))))

    val moduleSignature =
      val (_, symbol) = synthesizeModuleSignature(module)
      val name = if module.isClassDef && module.isModuleDef then module.companionModule.name else module.name
      val rhs = module.owner findAncestor isMultitierModule match
        case Some(outer) =>
          val (symbol, _) = synthesizeAccessors(outer).signature
          Ref(symbols.moduleSignatureNested).appliedTo(This(outer).select(symbol), Literal(StringConstant(name)))
        case _ =>
          Ref(symbols.moduleSignature).appliedTo(Literal(StringConstant(name)))
      ValDef(symbol, Some(rhs))

    val peerSignatures =
      PeerInfo.ofModule(module).iterator flatMap: peerInfo =>
        val peer = peerInfo.peerType.typeSymbol
        if peer != defn.AnyClass then
          val parents = peerInfo.parents flatMap { signature(_, peerInfo.pos) }

          val parentList = Typed(
            Repeated(parents, TypeTree.of(using types.peerSignature.asType)),
            TypeTree.of(using symbols.repeated.typeRef.appliedTo(types.peerSignature).asType))

          val signatureConstruction =
            Ref(symbols.peerSignature).appliedTo(
              Literal(StringConstant(peer.name)),
              Select.unique(Ref(symbols.list.companionModule), names.apply)
                .appliedToType(types.peerSignature)
                .appliedTo(parentList),
              Ref(moduleSignature.symbol))

          val ties = peerInfo.ties flatMap: (tie, multiplicity) =>
            signature(tie, peerInfo.pos) map: tie =>
              multiplicity match
                case Multiplicity.Single => Tuple(List(tie, Ref(symbols.peerTieSingle)))
                case Multiplicity.Optional => Tuple(List(tie, Ref(symbols.peerTieOptional)))
                case Multiplicity.Multiple => Tuple(List(tie, Ref(symbols.peerTieMultiple)))

          val tieList = Typed(
            Repeated(ties, TypeTree.of(using Tuple(List(types.peerSignature, types.peerTie)).asType)),
            TypeTree.of(using symbols.repeated.typeRef.appliedTo(Tuple(List(types.peerSignature, types.peerTie))).asType))

          val tiesConstruction =
            Select.unique(Ref(symbols.map.companionModule), names.apply)
              .appliedToTypes(List(types.peerSignature, types.peerTie))
              .appliedTo(tieList)

          val (signatureSymbol, tiesSymbol) = synthesizePeerSignature(module, peer)

          Some(
            peer ->
            (signatureSymbol,
             Some(ValDef(signatureSymbol, Some(signatureConstruction))),
             tiesSymbol,
             Some(DefDef(tiesSymbol, _ => Some(tiesConstruction)))))
        else
          None
    end peerSignatures

    ((moduleIdentifier.symbol, Some(moduleIdentifier)),
     (moduleSignature.symbol, Some(moduleSignature)),
     peerSignatures.to(SeqMap))
  end signatures

  def synthesizeAccessors(module: Symbol): Accessors = synthesizedAccessorsCache.getOrElse(module, {
    val tree =
      try module.tree
      catch case NonFatal(_) => Literal(NullConstant())
    tree match
      case tree: ClassDef => synthesizeAccessorsFromTree(module, tree)
      case _ => synthesizeAccessorsFromClass(module, classFileName(tree.symbol))
  })

  private def synthesizeAccessorsFromTree(module: Symbol, tree: ClassDef): Accessors =
    val mangledName = mangledSymbolName(module)
    val signaturePrefix =
      if module.isModuleDef then
        TypeToken(implementationForm(module)) :: TypeToken.` ` :: TypeToken.typeSignature(module.termRef)
      else
        TypeToken(implementationForm(module)) :: TypeToken.` ` :: TypeToken.typeSignature(module.typeRef)

    val (identifier @ (identifierSymbol, _), signature @ (signatureSymbol, _), peers) = signatures(module)

    val allowResolutionFailureForTypeParameters = module.flags is Flags.Final

    val defaultAccessorGeneration = if module.flags is Flags.Final then Required else Preferred

    val accessorGeneration =
      multitierModuleArgument(module) match
        case Some(arg) =>
          val symbol = arg.symbol
          val accessorGeneration =
            if symbol == TypeRepr.of[Deferred.type].termSymbol then Deferred
            else if symbol == TypeRepr.of[Preferred.type].termSymbol then Preferred
            else if symbol == TypeRepr.of[Required.type].termSymbol then Required
            else if symbol == TypeRepr.of[Forced.type].termSymbol then Forced
            else
              errorAndCancel("Unexpected accessor generation mode.", arg.posInUserCode)
              defaultAccessorGeneration

          if (module.flags is Flags.Final) && accessorGeneration != Required && accessorGeneration != Forced then
            val impl = if module.isModuleDef then "objects" else "final classes"
            errorAndCancel(s"Accessor generation mode for $impl must be `Required` or `Forced`.", arg.posInUserCode)

          accessorGeneration
        case _ =>
          defaultAccessorGeneration
    end accessorGeneration

    def isLocalVariable(symbol: Symbol) =
      symbol.maybeOwner != module && (symbol hasAncestor module)

    type AccessCollection = (List[TypeToken], Int, List[(Option[Symbol], Position, String, TypeRepr)], List[Term], Set[Symbol])

    object accessCollector extends TreeAccumulator[AccessCollection]:
      def foldTree(accesses: AccessCollection, tree: Tree)(owner: Symbol) =
        val (indexing, index, values, transmittables, accessed) = accesses
        tree match
          case ValDef(_, tpt, rhs) if !(tpt.tpe =:= TypeRepr.of[Nothing]) && tpt.tpe <:< types.transmittable =>
            rhs.fold(accesses):
              foldOverTree((indexing, index, values, transmittables, accessed), _)(owner)

          case DefDef(_, List() | List(List()), tpt, rhs) if !(tpt.tpe =:= TypeRepr.of[Nothing]) && tpt.tpe <:< types.transmittable =>
            rhs.fold(accesses):
              foldOverTree((indexing, index, values, transmittables, accessed), _)(owner)

          case PlacedBlockInvocation(value, captures, pos) =>
            val params = captures map { capture => PlacementInfo(capture).fold(capture) { _.valueType } }
            val signature = accessorSignature(indexing ++ List(TypeToken.number(index), TypeToken.`>`), List(params), value)
            val tpe = MethodType(captures.indices.toList map { index => s"arg$index" })(_ => params, _ => value)
            foldOverTree((indexing, index + 1, (None, pos, signature, tpe) :: values, transmittables, accessed), tree)(owner)

          case PlacedAccess(_, PlacedValueReference(value, _), _, _, _, _, _) if value.symbol.exists =>
            foldOverTree((indexing, index, values, transmittables, accessed + value.symbol), tree)(owner)

          case tree: Term if !(tree.tpe =:= TypeRepr.of[Nothing]) && tree.tpe <:< types.transmittable && !isLocalVariable(tree.symbol) =>
            foldOverTree((indexing, index, values, tree :: transmittables, accessed), tree)(owner)

          case _ =>
            foldOverTree(accesses, tree)(owner)
    end accessCollector

    def collectAccesses(indexing: String | Int, tree: Tree, values: List[(Option[Symbol], Position, String, TypeRepr)], transmittables: List[Term], accessed: Set[Symbol]) =
      val init = indexing match
        case index: Int => (TypeToken.`<` :: signaturePrefix ++ List(TypeToken.` `, TypeToken("nested"), TypeToken.` `), index)
        case name: String => (List(TypeToken(name), TypeToken.`<`, TypeToken("nested"), TypeToken.` `), 0)
      val (_, index, collectedValues, collectedTransmittables, collectedAccesses) =
        accessCollector.foldTree(init ++ (values, transmittables, accessed), tree)(module)
      (index, collectedValues, collectedTransmittables, collectedAccesses)

    val (_, values, transmittables, accessed) =
      tree.body.foldLeft(0, List.empty[(Option[Symbol], Position, String, TypeRepr)], List.empty[Term], Set.empty[Symbol]):
        case ((index, values, transmittables, accessed), stat @ (_: ValDef | _: DefDef))
            if (stat.symbol.isField || stat.symbol.isMethod) && !stat.symbol.isModuleDef =>
          val (tpt, rhs) = stat match
            case ValDef(_, tpt, rhs) => (tpt, rhs)
            case DefDef(_, _, tpt, rhs) => (tpt, rhs)

          val name = targetName(stat.symbol)
          val (_, collectedValues, collectedTransmittables, collectedAccessed) =
            collectAccesses(name, stat, values, transmittables, accessed)

          val value = PlacementInfo(tpt.tpe) collect:
            case placementInfo if !placementInfo.modality.local =>
              val params = stat.symbol.paramSymss collect:
                case params if params.isEmpty || params.head.isTerm => params map { _.info }

              inline def posWithoutBody = rhs map: rhs =>
                val start = rhs.posInUserCode.start
                val offset = stat.pos.sourceFile.content.fold(0): content =>
                  (content.substring(0, start).reverseIterator takeWhile { c => c.isWhitespace || c == '=' }).size
                Position(stat.pos.sourceFile, stat.posInUserCode.start, start - offset)

              val pos =
                stat.symbol.pos orElse
                posWithoutBody getOrElse
                Position(stat.pos.sourceFile, stat.posInUserCode.start, tpt.posInUserCode.end)

              (Some(stat.symbol),
               pos,
               accessorSignature(name, params, placementInfo.valueType),
               stat.symbol.info.withResultType(placementInfo.valueType))

          def insertedIntoNonEmptyListBefore[T](list: List[T], element: T, before: List[T]): List[T] =
            if list eq before then element :: list else list.head :: insertedIntoNonEmptyListBefore(list.tail, element, before)

          val insertedValues =
            value.fold(collectedValues): value =>
              if collectedValues.isEmpty then List(value) else insertedIntoNonEmptyListBefore(collectedValues, value, values)

          (index, insertedValues, collectedTransmittables, collectedAccessed)

        case ((index, values, transmittables, accessed), stat: Term) =>
          collectAccesses(index, stat, values, transmittables, accessed)

        case (accesses, _) =>
          accesses
    end val

    val transmittableTypeMap = MutableCachedTypeSeqMap[Transmittable]

    def incoherenceMessage(tpe: TypeRepr) =
      s"Incoherent transmittables for type ${tpe.safeShow(Printer.SafeTypeReprShortCode)}"

    transmittables.reverseIterator foreach: tree =>
      if !canceled then
        val transmittable = Transmittable(tree)
        transmittableTypeMap.lookupType(transmittable.types.base) match
          case Some(other) =>
            if !(transmittable.tree.tpe =:= other.tree.tpe) || transmittable.signature != other.signature then
              if !(transmittable.types.base =:= other.types.base) ||
                 !(transmittable.types.intermediate =:= other.types.intermediate) ||
                 !(transmittable.types.result =:= other.types.result) then
                errorAndCancel(
                  s"${incoherenceMessage(other.types.base)}. Found ${other.types.show} and ${transmittable.types.show}.",
                  tree.posInUserCode)
              else if !(transmittable.types.proxy =:= other.types.proxy) then
                errorAndCancel(
                  s"${incoherenceMessage(other.types.base)}. Found ${other.types.showMore} and ${transmittable.types.showMore}.",
                  tree.posInUserCode)
              else
                errorAndCancel(
                  s"${incoherenceMessage(other.types.base)} with type ${transmittable.types.showMore}.",
                  tree.posInUserCode)
          case _ =>
            transmittableTypeMap.addNewTypeEntry(transmittable.types.base, transmittable)

    val marshallableTypeMap = MutableCachedTypeSeqMap[Symbol]

    if !canceled then
      module.typeRef.baseClasses.tail foreach: parent =>
        if isMultitierModule(parent) then
          synthesizeAccessors(parent).marshalling.iterator foreach: (symbol, _) =>
            val tpe = ThisType(module).memberType(symbol)
            val types = MarshallableTypes(tpe)
            if marshallableTypeMap.lookupType(types.base).isEmpty then
              marshallableTypeMap.addNewTypeEntry(types.base, symbol)

    val serializableTypeMap = mutable.Map.empty[TypeRepr, Term]

    def marshallableConstruction(transmittable: Transmittable) =
      if transmittable.signature != abstractSignature then
        def contextBuilders(tpe: TypeRepr): Either[String, Term] =
          tpe.typeArgs match
            case List(tail, head) =>
              val headTypes = TransmittableTypes(head)
              contextBuilder(headTypes) flatMap: builder =>
                contextBuilders(tail) map: builders =>
                  Ref(symbols.listContext).appliedToTypes(headTypes.typeList :+ tail).appliedTo(builder, builders)
            case _ =>
              val types = TransmittableTypes(tpe)
              contextBuilder(types) map:
                Ref(symbols.delegateContext).appliedToTypes(types.typeList).appliedTo(_)

        def contextBuilder(types: TransmittableTypes): Either[String, Term] =
          if types.transmittables derivesFrom symbols.delegates then
            val delegating = types.transmittables.baseType(symbols.delegates).typeArgs.head
            contextBuilders(delegating) map:
              Ref(symbols.delegatingContext).appliedToType(delegating).appliedTo(_)
          else if types.transmittables derivesFrom symbols.message then
            val transmittable = types.transmittables.baseType(symbols.message).typeArgs.head
            val transmittableTypes = TransmittableTypes(transmittable)
            contextBuilder(transmittableTypes) flatMap: builder =>
              Resolution.resolveSerializable(transmittableTypes.intermediate) map: serializer =>
                Ref(symbols.messagingContext).appliedToTypes(transmittableTypes.typeList).appliedTo(builder, serializer)
          else if types.transmittables derivesFrom symbols.none then
            Right(Ref(symbols.noneContext))
          else
            Left(s"${types.base.safeShow(Printer.SafeTypeReprShortCode)} is not transmittable")

        contextBuilder(transmittable.types) flatMap: builder =>
          Resolution.resolveSerializable(transmittable.types.intermediate) map: serializer =>
            Some(Ref(symbols.marshallableResolution).appliedToTypes(transmittable.types.typeList).appliedTo(transmittable.tree, serializer, builder))
      else
        Right(None)
    end marshallableConstruction

    var marshallableIndex = 0

    enum RequiredMarshallable(val maybeResult: Option[TypeRepr], val maybeProxy: Option[TypeRepr]):
      def base: TypeRepr
      case Base(base: TypeRepr) extends RequiredMarshallable(None, None)
      case Result(base: TypeRepr, result: TypeRepr) extends RequiredMarshallable(Some(result), None)
      case Proxy(base: TypeRepr, proxy: TypeRepr) extends RequiredMarshallable(None, Some(proxy))

    def generateMarshallable(required: RequiredMarshallable, allowAbstract: Boolean) =
      val requiredTypes = TransmittableTypes(
        required.base,
        TypeBounds.empty,
        required.maybeResult getOrElse TypeBounds.empty,
        required.maybeProxy getOrElse TypeBounds.empty,
        TypeBounds.empty)

      def generateMarshallableName() =
        val name = s"$$loci$$marshalling$$$mangledName$$$marshallableIndex"
        marshallableIndex += 1
        name

      def generateMarshallable(transmittable: Transmittable, rhs: Option[Term]) =
        val info = symbols.marshallable.typeRef.appliedTo(transmittable.types.asMarshallableTypes.typeList)
        val flags = Flags.Lazy | (if rhs.isEmpty then Flags.Deferred else Flags.EmptyFlags)
        val symbol = newVal(module, generateMarshallableName(), info, flags, Symbol.noSymbol)
        val marshallable = ValDef(symbol, rhs map { _.changeOwner(symbol) })
        transmittable.marshallable = Some(marshallable)
        symbol

      def conformsToPredefinedMarshallable(base: TypeRepr) =
        required.base =:= base &&
        (required.maybeResult forall { _ =:= base }) &&
        (required.maybeProxy forall { _ =:= symbols.future.typeRef.appliedTo(base) })

      if conformsToPredefinedMarshallable(TypeRepr.of[Unit]) then
        Right(() => symbols.marshallableUnit)
      else if conformsToPredefinedMarshallable(TypeRepr.of[Null]) then
        Right(() => symbols.marshallableNull)
      else if conformsToPredefinedMarshallable(TypeRepr.of[Nothing]) then
        Right(() => symbols.marshallableNothing)
      else
        val initialResolutionFailure =
          if accessorGeneration == Forced && transmittableTypeMap.lookupType(required.base).isEmpty then
            val transmittable = Resolution.resolveTransmittable(requiredTypes.base, allowResolutionFailureForTypeParameters)
            transmittable foreach: transmittable =>
              transmittableTypeMap.addNewTypeEntry(transmittable.types.base, transmittable)
            transmittable.left.toOption
          else
            None

        transmittableTypeMap.lookupType(required.base) match
          case Some(transmittable) =>
            val conforms =
              (required.maybeResult forall { _ =:= transmittable.types.result }) &&
              (required.maybeProxy forall { _ =:= transmittable.types.proxy })

            if conforms then
              transmittable.marshallable match
                case Some(marshallable) =>
                  Right(() => marshallable.symbol)

                case _ =>
                  val marshallable =
                    marshallableTypeMap.lookupType(required.base) flatMap: marshallable =>
                      val types = MarshallableTypes(marshallable.info)
                      val signature = marshallableSignature(marshallable)
                      val conforms =
                        types.result =:= transmittable.types.result &&
                        types.proxy =:= transmittable.types.proxy &&
                        (accessorGeneration != Forced || signature == transmittable.signature && signature != unknownSignature)
                      Option.when(conforms) { Right(() => marshallable) }

                  marshallable getOrElse:
                    Resolution.Result(transmittable.tree).asTransmittable(allowResolutionFailureForTypeParameters) flatMap: transmittable =>
                      marshallableConstruction(transmittable) map: rhs =>
                        () => generateMarshallable(transmittable, rhs)
            else
              val message =
                if required.maybeProxy.nonEmpty then s"${incoherenceMessage(required.base)}. Found ${transmittable.types.showMore}, required ${requiredTypes.showMore}."
                else s"${incoherenceMessage(required.base)}. Found ${transmittable.types.show}, required ${requiredTypes.show}."
              Left(message)

          case _ =>
            val marshallable =
              if initialResolutionFailure.isEmpty then
                marshallableTypeMap.lookupType(required.base) flatMap: marshallable =>
                  val types = MarshallableTypes(marshallable.info)
                  val conforms =
                    (required.maybeResult forall { _ =:= types.result }) &&
                    (required.maybeProxy forall { _ =:= types.proxy })
                  Option.when(conforms) { Right(() => marshallable) }
              else
                None

            marshallable getOrElse:
              val result =
                initialResolutionFailure map { Left(_) } getOrElse:
                  Resolution.resolveTransmittable(requiredTypes.base, allowResolutionFailureForTypeParameters) flatMap: transmittable =>
                    marshallableConstruction(transmittable) map: rhs =>
                      () =>
                        transmittableTypeMap.addNewTypeEntry(transmittable.types.base, transmittable)
                        generateMarshallable(transmittable, rhs)

              if result.isLeft && allowAbstract then
                Right: () =>
                  val transmittable = Transmittable(Literal(NullConstant()), requiredTypes, abstractSignature)
                  transmittableTypeMap.addNewTypeEntry(requiredTypes.base, transmittable)
                  generateMarshallable(transmittable, None)
              else
                result
    end generateMarshallable

    def meaningfulType(tpe: TypeRepr) =
      tpe.typeSymbol != defn.UnitClass && tpe.typeSymbol != defn.NullClass && tpe.typeSymbol != defn.NothingClass

    def argumentTypes(tpe: TypeRepr): List[TypeRepr] = tpe match
      case MethodType(_, paramTypes, resType) =>
        (paramTypes filter meaningfulType) ++ argumentTypes(resType)
      case PolyType(_, _, resType) =>
        argumentTypes(resType)
      case _ =>
        List.empty

    val allowAbstract = accessorGeneration == Deferred || accessorGeneration == Preferred

    var placedIndex = 0
    var anonymousPlacedIndex = 0

    val accessors = values.reverseIterator flatMap: (original, pos, signature, tpe) =>
      val valueAccessed = original forall { accessed contains _ }

      if !canceled && (valueAccessed || accessorGeneration != Deferred) then
        val arguments = argumentTypes(tpe)
        val argumentType = if arguments.nonEmpty then Tuple(arguments) else TypeRepr.of[Unit]
        val resultType = tpe.resultType

        def marshallable(tpe: TypeRepr, required: TypeRepr => RequiredMarshallable) =
          generateMarshallable(required(tpe), allowAbstract && !valueAccessed) map { generate => () => Ref(generate()) }

        val generateMarshallables =
          marshallable(argumentType, tpe => RequiredMarshallable.Result(tpe, tpe)) flatMap: generateArgumentMarshallable =>
            marshallable(resultType, tpe => RequiredMarshallable.Base(tpe)) map: generateResultMarshallable =>
              (generateArgumentMarshallable, generateResultMarshallable)

        generateMarshallables match
          case Left(message) =>
            if valueAccessed || accessorGeneration != Preferred then
              errorAndCancel(message, pos)
            None

          case Right(generateArgumentMarshallable, generateResultMarshallable) =>
            val argumentMarshallable = generateArgumentMarshallable()
            val resultMarshallable = generateResultMarshallable()

            val argumentTypes = MarshallableTypes(argumentMarshallable.symbol.info)
            val resultTypes = MarshallableTypes(resultMarshallable.symbol.info)

            val name = s"$$loci$$placed$$$mangledName$$$placedIndex"
            placedIndex += 1

            val signatureConstruction =
              Ref(symbols.valueSignature).appliedTo(
                Literal(StringConstant(signature)),
                Ref(identifierSymbol),
                Ref(signatureSymbol).select(symbols.valueSignaturePath))

            val info = symbols.placedValue.typeRef.appliedTo(List(argumentTypes.base, argumentTypes.result, resultTypes.base, resultTypes.proxy))
            val symbol = newVal(module, name, info, Flags.Final, Symbol.noSymbol)
            val rhs = New(TypeIdent(symbols.placedValue)).select(symbols.placedValue.primaryConstructor).appliedToTypes(info.typeArgs).appliedTo(
              signatureConstruction,
              Literal(BooleanConstant(original exists { _.isStable })),
              argumentMarshallable,
              resultMarshallable)

            val key = original getOrElse:
              val key = anonymousPlacedIndex
              anonymousPlacedIndex += 1
              key

            Some(key -> (symbol, Some(ValDef(symbol, Some(rhs)))))
      else
        None
    end accessors

    val placed = accessors.to(SeqMap)

    val marshalling =
      transmittableTypeMap flatMapValues: transmittable =>
        transmittable.marshallable map: marshallable =>
          (marshallable.symbol, Some(marshallable))

    Accessors(identifier, signature, peers, marshalling, placed)
  end synthesizeAccessorsFromTree

  private def synthesizeAccessorsFromClass(module: Symbol, name: String): Accessors =
    val (identifier, signature, peers) = signatures(module)

    Accessors(
      identifier,
      signature,
      peers,
      CachedTypeSeqMap[(Symbol, Option[ValDef])],
      SeqMap.empty[Symbol | Int, (Symbol, Option[ValDef])])
  end synthesizeAccessorsFromClass
end RemoteAccessorSynthesis
