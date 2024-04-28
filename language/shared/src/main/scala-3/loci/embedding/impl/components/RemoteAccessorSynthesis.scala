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
import scala.math.Ordering
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
    overridden: List[(Symbol, Option[ValDef])],
    marshalling: CachedTypeSeqMap[(Symbol, Option[ValDef])],
    placed: SeqMap[Symbol | Int, (Symbol, Option[ValDef])])

  private val synthesizedModuleSignatureCache = RemoteAccessorSynthesis.synthesizedModuleSignatureCache match
    case cache: mutable.Map[Symbol, (Symbol, Symbol)] @unchecked => cache
  private val synthesizedPeerSignatureCache = RemoteAccessorSynthesis.synthesizedPeerSignatureCache match
    case cache: mutable.Map[(Symbol, Symbol), (Symbol, Symbol)] @unchecked => cache
  private val synthesizedAccessorsCache = RemoteAccessorSynthesis.synthesizedAccessorsCache match
    case cache: mutable.Map[Symbol, Accessors] @unchecked => cache

  def synthesizeModuleSignature(module: Symbol) = synthesizedModuleSignatureCache.getOrElseUpdate(module, {
    val hasMultitierParent = module.typeRef.baseClasses.tail exists isMultitierModule
    val flags = Flags.Lazy | (if hasMultitierParent then Flags.Override else Flags.EmptyFlags)
    val identifier = newVal(module, "$loci$mod", TypeRepr.of[String], flags, Symbol.noSymbol)
    val signature = newVal(module, "$loci$sig", types.moduleSignature, flags, Symbol.noSymbol)
    SymbolMutator.getOrErrorAndAbort.enter(module, identifier)
    SymbolMutator.getOrErrorAndAbort.enter(module, signature)
    (identifier, signature)
  })

  def synthesizePeerSignature(module: Symbol, peer: Symbol) = synthesizedPeerSignatureCache.getOrElseUpdate((module, peer), {
    val overridden = (peer.allOverriddenSymbols map { _.owner }).toSet + peer.owner
    val isOverriddingPeer = module.typeRef.baseClasses.tail exists { overridden contains _ }
    val overridingFlags = if isOverriddingPeer then Flags.Override else Flags.EmptyFlags
    val info = ByNameType(symbols.map.typeRef.appliedTo(List(types.peerSignature, types.peerTie)))
    val signature = newVal(module, s"$$loci$$peer$$sig$$${peer.name}", types.peerSignature, Flags.Lazy | overridingFlags, Symbol.noSymbol)
    val ties = newMethod(module, s"$$loci$$peer$$ties$$${peer.name}", info, overridingFlags, Symbol.noSymbol)
    SymbolMutator.getOrErrorAndAbort.enter(module, signature)
    SymbolMutator.getOrErrorAndAbort.enter(module, ties)
    (signature, ties)
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
      val tpe =
        if resultType derivesFrom symbols.transmittable then
          resultType.select(symbols.`type`).dealias
        else
          resultType
      val typeArgs =
        List(symbols.base, symbols.intermediate, symbols.result, symbols.proxy, symbols.transmittables) map: member =>
          tpe.resolvedMemberType(member) getOrElse TypeRepr.of[Any] match
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

  private class Transmittable(val tree: Term, val types: TransmittableTypes, var signature: String, var marshallable: Option[(Marshallable, ValDef)])

  private object Transmittable:
    def apply(tree: Term, types: TransmittableTypes, signature: String): Transmittable =
      new Transmittable(tree, types, signature, None)
    def apply(tree: Term): Transmittable =
      new Transmittable(tree, TransmittableTypes(tree.tpe), transmittableSignature(tree), None)

  private case class Marshallable(symbol: Symbol, types: MarshallableTypes, signature: String)

  private object Marshallable:
    def apply(symbol: Symbol, module: Symbol): Marshallable =
      val signature = marshallableInfo(symbol).fold(unknownSignature) { (signature, _, _, _) => signature }
      new Marshallable(symbol, MarshallableTypes(ThisType(module).select(symbol)), signature)

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

  private object AnnotationString:
    inline def unapply(term: Term) = term match
      case Inlined(_, _, Literal(StringConstant(value))) => Some(value)
      case Literal(StringConstant(value)) => Some(value)
      case _ => None

  private def marshallableInfo(symbol: Symbol) =
    symbol.getAnnotation(symbols.marshallableInfo) match
      case Some(Apply(_, List(AnnotationString(signature), AnnotationString(base), AnnotationString(result), AnnotationString(proxy)))) =>
        Some(signature, base, result, proxy)
      case _ =>
        None

  private def placedInfo(symbol: Symbol) =
    symbol.getAnnotation(symbols.placedValueInfo) match
      case Some(Apply(_, List(AnnotationString(signature), AnnotationString(arguments), AnnotationString(result)))) =>
        Some(signature, arguments, result)
      case _ =>
        None

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
        extension (self: String) def withPeriod =
          val trimmed = self.trim
          if trimmed.nonEmpty && trimmed.last == '.' then trimmed else trimmed + '.'
        resolutionFailureCollector.foldTree(None, tree)(Symbol.noSymbol) match
          case Some(message, false) => Failure(message.withPeriod)
          case Some(message, true) => FailureOnTypeParameter(message.withPeriod, tree)
          case _ => Success(tree)

      extension (self: Result)
        def asTransmittable(allowFailureForTypeParameters: Boolean) = self match
          case Success(term) => Right(Transmittable(term))
          case Failure(message) => Left(message)
          case FailureOnTypeParameter(message, term) =>
            Either.cond(
              allowFailureForTypeParameters,
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
        s"${tpe.safeShow(Printer.SafeTypeReprShortCode)} is not serializable.").asTerm

    def resolveTransmittable(tpe: TypeRepr, allowFailureForTypeParameters: Boolean) =
      resolve(
        symbols.transmittable.typeRef.appliedTo(List(tpe, TypeBounds.empty, TypeBounds.empty, TypeBounds.empty, TypeBounds.empty)),
        s"${tpe.safeShow(Printer.SafeTypeReprShortCode)} is not transmittable.").asTransmittable(allowFailureForTypeParameters)
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
        case Some(term) if isMultitierModule(term.symbol) =>
          val(symbol, _) = synthesizePeerSignature(peerType.typeSymbol.owner, peerType.typeSymbol)
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

  private def synthesizeAllPlacedAccessors(module: Symbol, includeFirst: Boolean): Map[Int | Symbol, Symbol] =
    val baseClasses = module.typeRef.baseClasses
    val placedTail = baseClasses.tail.reverseIterator flatMap: base =>
      if isMultitierModule(base) then
        synthesizeAccessors(base).placed.iterator flatMap:
          case (_: Int, _) => None
          case (original, (placed, _)) => Some(original -> placed)
      else
        Iterator.empty
    val placedHead =
      if includeFirst && isMultitierModule(baseClasses.head) then
        synthesizeAccessors(baseClasses.head).placed.iterator map:
          case (original, (placed, _)) => original -> placed
      else
        Iterator.empty
    (placedTail ++ placedHead).toMap

  def synthesizeAllPlacedAccessors(module: Symbol): Map[Int | Symbol, Symbol] =
    synthesizeAllPlacedAccessors(module, includeFirst = true)

  def synthesizeAccessors(module: Symbol): Accessors = synthesizedAccessorsCache.getOrElseUpdate(module, {
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

    val allowResolutionFailureForTypeParameters = !(module.flags is Flags.Final)

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

    def accessorGenerationFailureMessageProlog(symbolForName: Option[Symbol], symbolForParent: Option[Symbol], noninheritedPosition: Option[Position]) =
      val message = "Failed to generate accessor for"

      def signature(symbol: Symbol) =
        val result =
          val tpe = symbol.info.resultType
          PlacementInfo(tpe).fold(tpe.safeShow(Printer.SafeTypeReprShortCode)) { _.showCanonical }
        val args =
          symbol.paramSymss map: params =>
            val args = params map: param =>
              param.info match
                case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass && hi.typeSymbol == defn.AnyClass => param.name
                case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass => s"${param.name} <: ${hi.safeShow(Printer.SafeTypeReprShortCode)}"
                case TypeBounds(low, hi) if hi.typeSymbol == defn.AnyClass => s"${param.name} >: ${low.safeShow(Printer.SafeTypeReprShortCode)}"
                case TypeBounds(low, hi) => s"${param.name} >: ${low.safeShow(Printer.SafeTypeReprShortCode)} <: ${hi.safeShow(Printer.SafeTypeReprShortCode)}"
                case tpe => s"${param.name}: ${tpe.safeShow(Printer.SafeTypeReprShortCode)}"
            if params.isEmpty || params.head.isTerm then s"(${args.mkString(", ")})" else s"[${args.mkString(", ")}]"
        s"${args.mkString}: $result"

      val name = symbolForName match
        case Some(symbol) =>
          if symbol.name startsWith "$loci$anon$" then "remote block"
          else if symbol.isClassDef && symbol.isModuleDef then s"placed value ${symbol.companionModule.name}"
          else if symbol.isMethod then s"placed value ${symbol.name}${signature(symbol)}"
          else s"placed value ${symbol.name}"
        case _ =>
          "remote block"

      noninheritedPosition match
        case Some(pos) =>
          (s"$message $name", pos)
        case _ =>
          val parent =
            symbolForParent flatMap: symbol =>
              val parent = tree.parents find: parent =>
                parent.symbol == symbol.maybeOwner
              parent orElse:
                tree.parents find: parent =>
                  (parent.symbol.fieldMembers contains symbol) ||
                  (parent.symbol.methodMembers contains symbol) ||
                  (parent.symbol.typeMembers contains symbol)
          parent.fold(s"$message inherited $name", tree.posInUserCode.startPosition): parent =>
            symbolForParent match
              case Some(symbol) if parent.symbol == symbol.maybeOwner =>
                (s"$message $name, inherited from ${parent.symbol.name}", parent.posInUserCode)
              case Some(symbol) if symbol.maybeOwner.exists && symbol.maybeOwner.maybeOwner == module.maybeOwner =>
                (s"$message $name, defined in ${symbol.maybeOwner.name}, inherited through ${parent.symbol.name}", parent.posInUserCode)
              case Some(symbol) if symbol.maybeOwner.exists =>
                (s"$message $name, defined in ${fullName(symbol.maybeOwner)}, inherited through ${parent.symbol.name}", parent.posInUserCode)
              case _ =>
                (s"$message $name, inherited through ${parent.symbol.name}", parent.posInUserCode)
    end accessorGenerationFailureMessageProlog

    def marshallingIdentifier(name: String) =
      val identifier = name.stripPrefix("$loci$marshalling$").replace('$', ':')
      if identifier.length != name.length then identifier else s"<$identifier>"

    def isLocalVariable(symbol: Symbol) =
      symbol hasAncestor module

    type AccessCollection = (List[TypeToken], Int, List[(Option[Symbol], String, TypeRepr, () => (String, Position))], List[(Term, Position)], Set[Symbol], Option[Position])

    object accessCollector extends TreeAccumulator[AccessCollection]:
      def foldTree(accesses: AccessCollection, tree: Tree)(owner: Symbol) =
        val (indexing, index, values, transmittables, accessed, pos) = accesses
        tree match
          case ValDef(_, tpt, rhs) if !(tpt.tpe =:= TypeRepr.of[Nothing]) && tpt.tpe <:< types.transmittable =>
            rhs.fold(accesses):
              foldOverTree((indexing, index, values, transmittables, accessed, pos), _)(owner)

          case DefDef(_, List() | List(List()), tpt, rhs) if !(tpt.tpe =:= TypeRepr.of[Nothing]) && tpt.tpe <:< types.transmittable =>
            rhs.fold(accesses):
              foldOverTree((indexing, index, values, transmittables, accessed, pos), _)(owner)

          case PlacedBlockInvocation(value, captures, position) =>
            val params = captures map { capture => PlacementInfo(capture).fold(capture) { _.valueType } }
            val signature = accessorSignature(indexing ++ List(TypeToken.number(index), TypeToken.`>`), List(params), value)
            val tpe = MethodType(captures.indices.toList map { index => s"arg$index" })(_ => params, _ => value)
            val prolog = () => accessorGenerationFailureMessageProlog(symbolForName = None, symbolForParent = None, noninheritedPosition = Some(position))
            foldOverTree((indexing, index + 1, (None, signature, tpe, prolog) :: values, transmittables, accessed, pos), tree)(owner)

          case PlacedAccess(_, PlacedValueReference(value, _), _, _, _, _, _) if value.symbol.exists =>
            foldOverTree((indexing, index, values, transmittables, accessed + value.symbol, pos), tree)(owner)

          case tree: Term if !(tree.tpe =:= TypeRepr.of[Nothing]) && tree.tpe <:< types.transmittable && !isLocalVariable(tree.symbol) =>
            val treePosition = tree.posInUserCode
            val position = if treePosition != Position.ofMacroExpansion then treePosition else pos getOrElse treePosition
            foldOverTree((indexing, index, values, (tree, position) :: transmittables, accessed, pos), tree)(owner)

          case Select(qualifier, _) =>
            val treePosition = tree.posInUserCode
            val position =
              Option.when(treePosition != Position.ofMacroExpansion):
                val qualifierPosition = qualifier.posInUserCode
                if treePosition != Position.ofMacroExpansion then
                  val offset = treePosition.sourceFile.content.fold(0): content =>
                    (content.substring(qualifierPosition.end).iterator takeWhile { c => c.isWhitespace || c == '.' }).size
                  Position(treePosition.sourceFile, qualifierPosition.end + offset, treePosition.end).endPosition
                else
                  treePosition.endPosition
            val accesses = foldOverTree((indexing, index, values, transmittables, accessed, position), tree)(owner)
            val prefix = accesses.take(accesses.size - 1)
            prefix :* pos

          case _ =>
            foldOverTree((indexing, index, values, transmittables, accessed, pos), tree)(owner)
    end accessCollector

    def collectAccesses(indexing: String | Int, tree: Tree, values: List[(Option[Symbol], String, TypeRepr, () => (String, Position))], transmittables: List[(Term, Position)], accessed: Set[Symbol]) =
      val init = indexing match
        case index: Int => (TypeToken.`<` :: signaturePrefix ++ List(TypeToken.` `, TypeToken("nested"), TypeToken.` `), index)
        case name: String => (List(TypeToken(name), TypeToken.`<`, TypeToken("nested"), TypeToken.` `), 0)
      val (_, index, collectedValues, collectedTransmittables, collectedAccesses, _) =
        accessCollector.foldTree(init ++ (values, transmittables, accessed, None), tree)(module)
      (index, collectedValues, collectedTransmittables, collectedAccesses)

    val (_, values, transmittables, accessed) =
      tree.body.foldLeft(0, List.empty[(Option[Symbol], String, TypeRepr, () => (String, Position))], List.empty[(Term, Position)], Set.empty[Symbol]):
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

              val prolog = () =>
                inline def posWithoutBody = rhs map: rhs =>
                  val start = rhs.posInUserCode.start
                  val offset = stat.pos.sourceFile.content.fold(0): content =>
                    (content.substring(0, start).reverseIterator takeWhile { c => c.isWhitespace || c == '=' }).size
                  Position(stat.pos.sourceFile, stat.posInUserCode.start, start - offset)

                val pos =
                  stat.symbol.pos orElse
                  posWithoutBody getOrElse
                  Position(stat.pos.sourceFile, stat.posInUserCode.start, tpt.posInUserCode.end)

                accessorGenerationFailureMessageProlog(symbolForName = Some(stat.symbol), symbolForParent = Some(stat.symbol), noninheritedPosition = Some(pos))
              end prolog

              (Some(stat.symbol),
               accessorSignature(name, params, placementInfo.valueType),
               stat.symbol.info.withResultType(placementInfo.valueType),
               prolog)
          end value

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

    object localVariablesCollector extends TreeAccumulator[(Set[Symbol], Boolean)]:
      def foldTree(variables: (Set[Symbol], Boolean), tree: Tree)(owner: Symbol) =
        val (localVariables, accessesLocalVariable) = variables
        tree match
          case tree: Definition =>
            foldOverTree((localVariables + tree.symbol, accessesLocalVariable), tree)(owner)
          case tree if tree.symbol.exists =>
            val symbol = tree.symbol
            if accessesLocalVariable || (!(localVariables contains symbol) && isLocalVariable(symbol) && (symbol.isTerm || symbol.owner != module)) then
              (localVariables, true)
            else
              foldOverTree(variables, tree)(owner)
          case tree =>
            foldOverTree(variables, tree)(owner)

    transmittables.reverseIterator foreach: (tree, pos) =>
      if !canceled then
        val (_, accessesLocalVariable) = localVariablesCollector.foldTree((Set.empty, false), tree)(Symbol.noSymbol)
        val transmittable = Transmittable(tree)
        if !accessesLocalVariable then
          transmittableTypeMap.lookupType(transmittable.types.base) match
            case Some(other) =>
              if !(transmittable.tree.tpe =:= other.tree.tpe) || transmittable.signature != other.signature then
                if !(transmittable.types.base =:= other.types.base) ||
                   !(transmittable.types.intermediate =:= other.types.intermediate) ||
                   !(transmittable.types.result =:= other.types.result) then
                  errorAndCancel(s"${incoherenceMessage(other.types.base)}. Found ${other.types.show} and ${transmittable.types.show}.", pos)
                else if !(transmittable.types.proxy =:= other.types.proxy) then
                  errorAndCancel(s"${incoherenceMessage(other.types.base)}. Found ${other.types.showMore} and ${transmittable.types.showMore}.", pos)
                else
                  errorAndCancel(s"${incoherenceMessage(other.types.base)} with type ${transmittable.types.showMore}.", pos)
            case _ =>
              transmittableTypeMap.addNewTypeEntry(transmittable.types.base, transmittable)
        else
          errorAndCancel(s"Illegal transmittable for type ${transmittable.types.base.safeShow(Printer.SafeTypeReprShortCode)} referring to local variable.", pos)

    val inheritedPlacedAccessors =
      synthesizeAllPlacedAccessors(module, includeFirst = false)

    SymbolMutator.getOrErrorAndAbort.invalidateMemberCaches(module)

    val inheritedValues =
      if !canceled then
        val overridden = mutable.Set.empty[Symbol]
        module.typeRef.baseClasses.tail flatMap: parent =>
          parent.declarations flatMap: decl =>
            if (decl.isMethod || decl.isField) &&
               (!(decl.flags is Flags.Synthetic) || (decl.name startsWith "$loci$anon$")) &&
               !(overridden contains decl) then
              val tpe = ThisType(module).memberType(decl)
              PlacementInfo(tpe.resultType) flatMap: placementInfo =>
                if !placementInfo.modality.local then
                  overridden ++= decl.allOverriddenSymbols
                  Option.when(accessorGeneration == Forced || !(inheritedPlacedAccessors contains decl)):
                    val name = targetName(decl)
                    val params = decl.paramSymss collect:
                      case params if params.isEmpty || params.head.isTerm => params map { _.info }
                    (Some(decl),
                     accessorSignature(name, params, placementInfo.valueType),
                     tpe.withResultType(placementInfo.valueType),
                     () => accessorGenerationFailureMessageProlog(symbolForName = Some(decl), symbolForParent = Some(decl), noninheritedPosition = None))
                else
                  None
            else
              None
      else
        List.empty

    val inheritedMarshallables = MutableCachedTypeSeqMap[mutable.SortedSet[Marshallable]]

    if !canceled then
      given Ordering[Marshallable] = (marshallable0, marshallable1) =>
        (marshallable0.signature, marshallable1.signature) match
          case (`abstractSignature`, signature1) if signature1 != abstractSignature => 1
          case (signature0, `abstractSignature`) if signature0 != abstractSignature => -1
          case (`unknownSignature`, signature1) if signature1 != unknownSignature => 1
          case (signature0, `unknownSignature`) if signature0 != unknownSignature => -1
          case (signature0, signature1) => signature0 compare signature1 match
            case 0 => marshallable0.symbol.name compare marshallable1.symbol.name
            case result => result

      val definedOrOverriddenMarshallables = mutable.Set.empty[String]
      module.typeRef.baseClasses.tail foreach: parent =>
        if isMultitierModule(parent) then
          val accessors = synthesizeAccessors(parent)
          accessors.overridden.iterator ++ accessors.marshalling.iterator foreach: (symbol, _) =>
            if !(definedOrOverriddenMarshallables contains symbol.name) then
              definedOrOverriddenMarshallables += symbol.name
              val marshallable = Marshallable(symbol, module)
              inheritedMarshallables.lookupType(marshallable.types.base) match
                case Some(marshallables) => marshallables += marshallable
                case _ => inheritedMarshallables.addNewTypeEntry(marshallable.types.base, mutable.SortedSet(marshallable))

    val serializableTypeMap = MutableCachedTypeSeqMap[Term]

    def resolveSerializable(tpe: TypeRepr) =
      serializableTypeMap.lookupType(tpe) match
        case Some(term) =>
          Right(term)
        case _ =>
          val serializable = Resolution.resolveSerializable(tpe)
          serializable foreach { serializableTypeMap.addNewTypeEntry(tpe, _) }
          serializable

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
              resolveSerializable(transmittableTypes.intermediate) map: serializer =>
                Ref(symbols.messagingContext).appliedToTypes(transmittableTypes.typeList).appliedTo(builder, serializer)
          else if types.transmittables derivesFrom symbols.none then
            Right(Ref(symbols.noneContext))
          else
            Left(s"${types.base.safeShow(Printer.SafeTypeReprShortCode)} is not transmittable")

        contextBuilder(transmittable.types) flatMap: builder =>
          resolveSerializable(transmittable.types.intermediate) map: serializer =>
            Some:
              Ref(types.marshallable.typeSymbol.companionModule)
                .select(symbols.marshallableResolution)
                .appliedToTypes(transmittable.types.typeList)
                .appliedTo(transmittable.tree, serializer, builder)
      else
        Right(None)
    end marshallableConstruction

    def marshallable(signature: String, types: MarshallableTypes, rhs: Option[Term], flags: Flags, generateName: () => String) =
      inline def serializeTypeAndSanityCheck(tpe: TypeRepr, from: Symbol) =
        TypeToken.serializeType(tpe, from) filter:
          TypeToken.deserializeType(_, from) exists { _ =:= tpe }

      val argsHead = Literal(StringConstant(signature))
      val argsTail = types.typeList.foldRight[Option[List[Term]]](Some(List.empty)):
        case (tpe, Some(types)) => serializeTypeAndSanityCheck(tpe, module) map { value => Literal(StringConstant(value)) :: types }
        case _ => None

      if argsTail.nonEmpty || (module hasAncestor { symbol => symbol.isMethod || symbol.isField }) || signature != abstractSignature then
        val symbol = newVal(
          module,
          generateName(),
          symbols.marshallable.typeRef.appliedTo(types.typeList),
          flags | Flags.Lazy | Flags.Protected | (if rhs.isEmpty then Flags.Deferred else Flags.EmptyFlags),
          Symbol.noSymbol)
        trySetThreadUnsafe(symbol)
        SymbolMutator.getOrErrorAndAbort.enter(module, symbol)
        argsTail foreach: argsTail =>
          SymbolMutator.getOrErrorAndAbort.updateAnnotation(symbol, symbols.marshallableInfo, argsHead :: argsTail)
        Right(Marshallable(symbol, types, signature), ValDef(symbol, rhs map { _.changeOwner(symbol) }))
      else
        Left(s"Failed to serialize type for ${TransmittableTypes(types.base, TypeBounds.empty, types.result, types.proxy, TypeBounds.empty).showMore}.")
    end marshallable

    val marshallableUnit = Marshallable(symbols.marshallableUnit, symbols.marshallableUnit.owner)
    val marshallableNull = Marshallable(symbols.marshallableNull, symbols.marshallableNull.owner)
    val marshallableNothing = Marshallable(symbols.marshallableNothing, symbols.marshallableNothing.owner)

    var marshallableIndex = 0

    enum RequiredMarshallable(val maybeResult: Option[TypeRepr], val maybeProxy: Option[TypeRepr]):
      def base: TypeRepr
      case Base(base: TypeRepr) extends RequiredMarshallable(None, None)
      case Result(base: TypeRepr, result: TypeRepr) extends RequiredMarshallable(Some(result), None)
      case Proxy(base: TypeRepr, result: TypeRepr, proxy: TypeRepr) extends RequiredMarshallable(Some(result), Some(proxy))

    def generateMarshallable(required: RequiredMarshallable, allowSkippingAbstract: Boolean, name: Option[String]): Either[String, () => Either[String, Option[Marshallable]]] =
      val requiredTypes = TransmittableTypes(
        required.base,
        TypeBounds.empty,
        required.maybeResult getOrElse TypeBounds.empty,
        required.maybeProxy getOrElse TypeBounds.empty,
        TypeBounds.empty)

      def generateMarshallableName() =
        name getOrElse:
          val name = s"$$loci$$marshalling$$$mangledName$$$marshallableIndex"
          marshallableIndex += 1
          name

      def generateMarshallable(transmittable: Transmittable, rhs: Option[Term]) =
        if transmittable.signature != abstractSignature || !allowSkippingAbstract then
          marshallable(transmittable.signature, transmittable.types.asMarshallableTypes, rhs, Flags.EmptyFlags, generateMarshallableName) map: (marshallable, definition) =>
            transmittable.marshallable = Some(marshallable, definition)
            Some(marshallable)
        else
          Right(None)

      def conformsToMarshallable(types: MarshallableTypes) =
        required.base =:= types.base &&
        (required.maybeResult forall { _ =:= types.result }) &&
        (required.maybeProxy forall { _ =:= types.proxy })

      def conformsToPredefinedMarshallable(base: TypeRepr) =
        conformsToMarshallable(MarshallableTypes(base, base, symbols.future.typeRef.appliedTo(base)))

      def checkTransmittableConformation[T](transmittable: Transmittable)(body: => Either[String, T]) =
        if conformsToMarshallable(transmittable.types.asMarshallableTypes) then
          body
        else
          val message = s"${transmittable.types.base.safeShow(Printer.SafeTypeReprShortCode)} is not transmittable"
          Left:
            if required.maybeProxy.nonEmpty then s"$message. Found ${transmittable.types.showMore}, required ${requiredTypes.showMore}."
            else s"$message. Found ${transmittable.types.show}, required ${requiredTypes.show}."

      if conformsToPredefinedMarshallable(TypeRepr.of[Unit]) then
        Right(() => Right(Some(marshallableUnit)))
      else if conformsToPredefinedMarshallable(TypeRepr.of[Null]) then
        Right(() => Right(Some(marshallableNull)))
      else if conformsToPredefinedMarshallable(TypeRepr.of[Nothing]) then
        Right(() => Right(Some(marshallableNothing)))
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
            checkTransmittableConformation(transmittable):
              transmittable.marshallable match
                case Some(marshallable, _) =>
                  Right(() => Right(Some(marshallable)))

                case _ =>
                  val marshallable =
                    inheritedMarshallables.lookupType(required.base) flatMap: marshallables =>
                      val marshallable = marshallables.head
                      Option.unless(name contains marshallable.symbol.name):
                        val conforms =
                          conformsToMarshallable(marshallable.types) &&
                          (accessorGeneration != Forced || marshallable.signature == transmittable.signature && marshallable.signature != unknownSignature)
                        Option.when(conforms) { Right(() => Right(Some(marshallable))) }

                  marshallable.flatten getOrElse:
                    Resolution.Result(transmittable.tree).asTransmittable(allowResolutionFailureForTypeParameters) flatMap: resolved =>
                      if marshallable.isEmpty && resolved.signature == abstractSignature then
                        Right(() => Right(None))
                      else
                        transmittable.signature = resolved.signature
                        marshallableConstruction(transmittable) map: rhs =>
                          () => generateMarshallable(transmittable, rhs)

          case _ =>
            val marshallable =
              if initialResolutionFailure.isEmpty then
                inheritedMarshallables.lookupType(required.base) flatMap: marshallables =>
                  val marshallable = marshallables.head
                  val conforms =
                    !(name contains marshallable.symbol.name) &&
                    conformsToMarshallable(marshallable.types)
                  Option.when(conforms) { Right(() => Right(Some(marshallable))) }
              else
                None

            marshallable getOrElse:
              val result =
                initialResolutionFailure map { Left(_) } getOrElse:
                  Resolution.resolveTransmittable(requiredTypes.base, allowResolutionFailureForTypeParameters) flatMap: transmittable =>
                    checkTransmittableConformation(transmittable):
                      marshallableConstruction(transmittable) map: rhs =>
                        () =>
                          transmittableTypeMap.addNewTypeEntry(transmittable.types.base, transmittable)
                          generateMarshallable(transmittable, rhs)

              if result.isLeft && allowSkippingAbstract then
                Right(() => Right(None))
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

    val allowSkippingAbstract = accessorGeneration == Deferred || accessorGeneration == Preferred

    var placedIndex = 0
    var anonymousPlacedIndex = 0

    val accessors = values.reverseIterator ++ inheritedValues flatMap: (original, signature, tpe, position) =>
      val valueAccessed = original forall { accessed contains _ }
      val valuePrivate = original forall { _.flags is Flags.Private }

      if !canceled && (valueAccessed || accessorGeneration != Deferred) then
        val arguments = argumentTypes(tpe)
        val argumentType =
          if arguments.isEmpty then TypeRepr.of[Unit]
          else if arguments.sizeIs == 1 then arguments.head
          else Tuple(arguments)
        val resultType = tpe.resultType

        def marshallable(required: RequiredMarshallable) =
          generateMarshallable(required, (allowSkippingAbstract || valuePrivate) && !valueAccessed, name = None)

        val marshallables =
          marshallable(RequiredMarshallable.Result(argumentType, argumentType)) flatMap: generateArgumentMarshallable =>
            marshallable(RequiredMarshallable.Base(resultType)) flatMap: generateResultMarshallable =>
              val argumentMarshallable = generateArgumentMarshallable()
              val resultMarshallable = generateResultMarshallable()
              argumentMarshallable flatMap: argumentMarshallable =>
                resultMarshallable map: resultMarshallable =>
                  argumentMarshallable flatMap: argumentMarshallable =>
                    resultMarshallable map: resultMarshallable =>
                      (argumentMarshallable, resultMarshallable)

        marshallables match
          case Left(message) =>
            if valueAccessed || accessorGeneration != Preferred then
              val (prolog, pos) = position()
              errorAndCancel(s"$prolog: $message", pos)
            None

          case Right(None) =>
            None

          case Right(Some(argumentMarshallable, resultMarshallable)) =>
            val argumentIdentifier = marshallingIdentifier(argumentMarshallable.symbol.name)
            val resultIdentifier = marshallingIdentifier(resultMarshallable.symbol.name)

            val inheritedPlacedWithIdenticalMarshallables =
              original exists: original =>
                Iterator(original) ++ original.allOverriddenSymbols collectFirst Function.unlift(inheritedPlacedAccessors.get) exists:
                  placedInfo(_) exists: (_, arguments, result) =>
                    arguments == argumentIdentifier && result == resultIdentifier

            Option.unless(inheritedPlacedWithIdenticalMarshallables):
              val name = s"$$loci$$placed$$$mangledName$$$placedIndex"
              placedIndex += 1

              val signatureConstruction =
                Ref(symbols.valueSignature).appliedTo(
                  Literal(StringConstant(signature)),
                  Ref(identifierSymbol),
                  Ref(signatureSymbol).select(symbols.valueSignaturePath))

              val info = symbols.placedValue.typeRef.appliedTo(
                List(
                  argumentMarshallable.types.base,
                  argumentMarshallable.types.result,
                  resultMarshallable.types.base,
                  resultMarshallable.types.proxy))
              val symbol = newVal(module, name, info, Flags.Final | Flags.Protected, Symbol.noSymbol)
              SymbolMutator.getOrErrorAndAbort.enter(module, symbol)

              inline def reference(symbol: Symbol) =
                if symbol.owner == types.marshallable.typeSymbol.companionModule.moduleClass then
                  Ref(symbol)
                else
                  This(module).select(symbol)

              val rhs = New(TypeIdent(symbols.placedValue)).select(symbols.placedValue.primaryConstructor).appliedToTypes(info.typeArgs).appliedTo(
                signatureConstruction,
                Literal(BooleanConstant(original exists { _.isStable })),
                reference(argumentMarshallable.symbol),
                reference(resultMarshallable.symbol))

              val argsHead = Literal(StringConstant(signature))
              val argsTail = List(argumentIdentifier, resultIdentifier) map { identifier => Literal(StringConstant(identifier)) }

              SymbolMutator.getOrErrorAndAbort.updateAnnotation(symbol, symbols.placedValueInfo, argsHead :: argsTail)

              val key = original getOrElse:
                val key = anonymousPlacedIndex
                anonymousPlacedIndex += 1
                key

              key -> (symbol, Some(ValDef(symbol, Some(rhs))))
      else
        None
    end accessors

    val placed = accessors.to(SeqMap)

    val overridingMarshallables = mutable.ListBuffer.empty[(Symbol, Option[ValDef])]

    if accessorGeneration == Required || accessorGeneration == Forced then
      def messageProlog(marshallable: Symbol) =
        val name = marshallingIdentifier(marshallable.name)
        val symbol = inheritedPlacedAccessors.iterator collectFirst Function.unlift:
          case (_: Int, _) =>
            None
          case (symbol: Symbol @unchecked, placed) =>
            placedInfo(placed) flatMap: (_, arguments, result) =>
              Option.when(arguments == name || result == name) { symbol }
        accessorGenerationFailureMessageProlog(symbolForName = symbol, symbolForParent = Some(marshallable), noninheritedPosition = None)

      inheritedMarshallables.iterator foreach:
        _ foreach:
          case Marshallable(symbol, types, signature) =>
            if signature == abstractSignature then
              val resolvedMarshallable = generateMarshallable(
                RequiredMarshallable.Proxy(types.base, types.result, types.proxy),
                allowSkippingAbstract = false,
                Some(symbol.name)) flatMap { _() }
              resolvedMarshallable match
                case Left(message) =>
                  val (prolog, pos) = messageProlog(symbol)
                  errorAndCancel(s"$prolog: $message", pos)
                case Right(None) =>
                case Right(Some(resolvedMarshallable)) =>
                  if resolvedMarshallable.symbol.name == symbol.name then
                    val definition = transmittableTypeMap.lookupType(types.base) flatMap:
                      _.marshallable map { (_, definition) => definition }
                    overridingMarshallables += resolvedMarshallable.symbol -> definition
                  else
                    val generatedMarshallable = marshallable(
                      resolvedMarshallable.signature,
                      resolvedMarshallable.types,
                      Some(This(module).select(resolvedMarshallable.symbol)),
                      Flags.Override,
                      () => symbol.name)
                    generatedMarshallable match
                      case Left(message) =>
                        val (prolog, pos) = messageProlog(symbol)
                        errorAndCancel(s"$prolog: $message", pos)
                      case Right(_, generatedMarshallable) =>
                        overridingMarshallables += generatedMarshallable.symbol -> Some(generatedMarshallable)

    val overriding = overridingMarshallables.result()

    val marshalling =
      val marshallables = (overriding.iterator map { (symbol, _) => symbol }).toSet
      transmittableTypeMap flatMapValues: transmittable =>
        transmittable.marshallable flatMap: (_, marshallable) =>
          Option.unless(marshallables contains marshallable.symbol):
            (marshallable.symbol, Some(marshallable))

    Accessors(identifier, signature, peers, overriding, marshalling, placed)
  end synthesizeAccessorsFromTree

  private def synthesizeAccessorsFromClass(module: Symbol, name: String): Accessors =
    val (identifier, signature, peers) = signatures(module)

    Accessors(
      identifier,
      signature,
      peers,
      List.empty,
      CachedTypeSeqMap[(Symbol, Option[ValDef])],
      SeqMap.empty[Symbol | Int, (Symbol, Option[ValDef])])
  end synthesizeAccessorsFromClass
end RemoteAccessorSynthesis
