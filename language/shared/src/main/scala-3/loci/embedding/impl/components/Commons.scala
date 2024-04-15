package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.compileTimeOnly
import scala.annotation.experimental
import scala.reflect.TypeTest
import scala.quoted.*

object Commons:
  @compileTimeOnly("Cannot be used at run time")
  transparent inline private def ?[T]: T =
    throw new NotImplementedError

  extension (expr: Expr[?])(using Quotes) private def symbol =
    import quotes.reflect.*
    expr.asTerm.underlyingArgument.symbol
end Commons

@experimental
trait Commons:
  this: Component =>
  import quotes.reflect.*

  import Commons.*

  object symbols:
    val `language.Local` = Symbol.requiredPackage("loci.language").typeMember("Local")
    val `language.per` = Symbol.requiredPackage("loci.language").typeMember("per")
    val `language.on` = Symbol.requiredPackage("loci.language").typeMember("on")
    val `embedding.on` = Symbol.requiredPackage("loci.embedding").typeMember("on")
    val `embedding.of` = Symbol.requiredPackage("loci.embedding").typeMember("of")
    val from = Symbol.requiredPackage("loci.embedding").typeMember("from")
    val fromSingle = Symbol.requiredPackage("loci.embedding").typeMember("fromSingle")
    val fromMultiple = Symbol.requiredPackage("loci.embedding").typeMember("fromMultiple")
    val base = TypeRepr.of[transmitter.Transmittable.Any[?, ?, ?]].typeSymbol.typeMember("Base")
    val intermediate = TypeRepr.of[transmitter.Transmittable.Any[?, ?, ?]].typeSymbol.typeMember("Intermediate")
    val result = TypeRepr.of[transmitter.Transmittable.Any[?, ?, ?]].typeSymbol.typeMember("Result")
    val proxy = TypeRepr.of[transmitter.Transmittable.Any[?, ?, ?]].typeSymbol.typeMember("Proxy")
    val transmittables = TypeRepr.of[transmitter.Transmittable.Any[?, ?, ?]].typeSymbol.typeMember("Transmittables")
    val `language.multitier` = TypeRepr.of[language.multitier].typeSymbol
    val `embedding.multitier` = TypeRepr.of[embedding.multitier].typeSymbol
    val on = TypeRepr.of[embedding.On[?]].typeSymbol
    val select = TypeRepr.of[embedding.Select[?]].typeSymbol
    val run = TypeRepr.of[embedding.Run[?, ?]].typeSymbol
    val capture = TypeRepr.of[embedding.Capture[?, ?, ?]].typeSymbol
    val block = TypeRepr.of[embedding.Block[?, ?, ?]].typeSymbol
    val placed = TypeRepr.of[Placed[?, ?]].typeSymbol
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]].typeSymbol
    val remote = TypeRepr.of[language.Remote[?]].typeSymbol
    val remoteApply = '{ language.remote.apply }.symbol
    val selectApplySingle = '{ ?[embedding.Select[?]].apply(?[language.Remote[?]]) }.symbol
    val selectApplyMultiple = '{ ?[embedding.Select[?]].apply(?[language.Remote[?]], ?[language.Remote[?]]) }.symbol
    val selectApplySeq = '{ ?[embedding.Select[?]].apply(?[Seq[language.Remote[?]]]) }.symbol
    val callApply = '{ ?[embedding.Call[?, ?]].call(?) }.symbol
    val peer = TypeRepr.of[language.peer].typeSymbol
    val single = TypeRepr.of[language.Single[?]].typeSymbol
    val optional = TypeRepr.of[language.Optional[?]].typeSymbol
    val multiple = TypeRepr.of[language.Multiple[?]].typeSymbol
    val context = TypeRepr.of[Placement.Context.type].typeSymbol
    val delegates = TypeRepr.of[transmitter.Transmittables.Delegates[?]].typeSymbol
    val message = TypeRepr.of[transmitter.Transmittables.Message[?]].typeSymbol
    val none = TypeRepr.of[transmitter.Transmittables.None].typeSymbol
    val delegatingContext = '{ transmitter.ContextBuilder.delegating(using ?) }.symbol
    val messagingContext = '{ transmitter.ContextBuilder.messaging(using ?, ?) }.symbol
    val noneContext = '{ transmitter.ContextBuilder.none }.symbol
    val delegateContext = '{ transmitter.ContextBuilders.delegate(using ?) }.symbol
    val listContext = '{ transmitter.ContextBuilders.list(using ?, ?) }.symbol
    val transmittable = TypeRepr.of[transmitter.Transmittable.Resolution[?, ?, ?, ?, ?]].typeSymbol
    val serializable = TypeRepr.of[serializer.Serializable[?]].typeSymbol
    val marshallable = TypeRepr.of[transmitter.Marshallable[?, ?, ?]].typeSymbol
    val marshallableResolution = '{ transmitter.Marshallable.marshallable(?, ?, ?) }.symbol
    val marshallableUnit = '{ transmitter.Marshallable.unit }.symbol
    val marshallableNull = '{ transmitter.Marshallable.`null` }.symbol
    val marshallableNothing = '{ transmitter.Marshallable.nothing }.symbol
    val transmission = TypeRepr.of[language.transmitter.Transmission.type].typeSymbol
    val placedValue = TypeRepr.of[loci.runtime.PlacedValue[?, ?, ?, ?]].typeSymbol
    val valueSignaturePath = '{ ?[loci.runtime.Value.Signature].path }.symbol
    val valueSignature = '{ loci.runtime.Value.Signature.apply(?, ?, ?) }.symbol
    val peerSignature = '{ loci.runtime.Peer.Signature.apply(?, ?, ?) }.symbol
    val peerTieSingle = '{ loci.runtime.Peer.Tie.Single }.symbol
    val peerTieOptional = '{ loci.runtime.Peer.Tie.Optional }.symbol
    val peerTieMultiple = '{ loci.runtime.Peer.Tie.Multiple }.symbol
    val moduleSignature = '{ loci.runtime.Module.Signature.apply(?[String]) }.symbol
    val moduleSignatureNested = '{ loci.runtime.Module.Signature.apply(?[loci.runtime.Module.Signature], ?[String]) }.symbol
    val erased = '{ embedding.erased }.symbol
    val erasedArgs = '{ embedding.erased(?) }.symbol
    val function1 = TypeRepr.of[Function1[?, ?]].typeSymbol
    val function1Apply = '{ ?[Function1[?, ?]].apply(?) }.symbol
    val contextFunction1 = TypeRepr.of[ContextFunction1[?, ?]].typeSymbol
    val contextFunction1Apply = '{ ?[ContextFunction1[?, ?]].apply(using ?) }.symbol
    val list = TypeRepr.of[List[?]].typeSymbol
    val map = TypeRepr.of[Map[?, ?]].typeSymbol
    val future = TypeRepr.of[concurrent.Future[?]].typeSymbol
    val contextResultCount = TypeRepr.of[annotation.internal.ContextResultCount].typeSymbol
    val compileTimeOnly = TypeRepr.of[annotation.compileTimeOnly].typeSymbol
    val targetName = TypeRepr.of[annotation.targetName].typeSymbol
    val asInstanceOf = '{ ?.asInstanceOf }.symbol
    val repeated = TypeRepr.of[`<repeated>`[?]].typeSymbol

  object types:
    val `language.on` = symbols.`language.on`.typeRef.appliedTo(List(TypeBounds.empty, TypeBounds.empty))
    val `embedding.on` = symbols.`embedding.on`.typeRef.appliedTo(List(TypeBounds.empty, TypeBounds.empty))
    val from = symbols.from.typeRef.appliedTo(List(TypeBounds.empty, TypeBounds.empty))
    val fromSingle = symbols.fromSingle.typeRef.appliedTo(List(TypeBounds.empty, TypeBounds.empty))
    val fromMultiple = symbols.fromMultiple.typeRef.appliedTo(List(TypeBounds.empty, TypeBounds.empty))
    val placedValue = TypeRepr.of[PlacedValue[?, ?]]
    val placed = TypeRepr.of[Placed[?, ?]]
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]]
    val remote = TypeRepr.of[language.Remote[?]]
    val context = TypeRepr.of[Placement.Context[?]]
    val contextResolutionWithFallback = TypeRepr.of[Placement.Context.ResolutionWithFallback[?]]
    val transmittable = TypeRepr.of[transmitter.Transmittable.Resolution[?, ?, ?, ?, ?]]
    val marshallable = TypeRepr.of[transmitter.Marshallable[?, ?, ?]]
    val transmission = TypeRepr.of[language.transmitter.Transmission[?, ?, ?, ?, ?]]
    val placedValues = TypeRepr.of[loci.runtime.PlacedValues]
    val valueSignature = TypeRepr.of[loci.runtime.Value.Signature]
    val peerSignature = TypeRepr.of[loci.runtime.Peer.Signature]
    val peerTie = TypeRepr.of[loci.runtime.Peer.Tie]
    val moduleSignature = TypeRepr.of[loci.runtime.Module.Signature]
    val system = TypeRepr.of[loci.runtime.System]
    val conversion = TypeRepr.of[Conversion[?, ?]]

  object names:
    val apply = "apply"
    val tie = "Tie"
    val system = "$loci$sys"
    val systemCreate = "$loci$sys$create"
    val resolutionFailure = "resolutionFailure"
    val anon = "$anon"

  object MaybeTyped:
    def unapply(term: Term): Some[Term] = term match
      case Typed(expr, _) => unapply(expr)
      case _ => Some(term)

  final class PackedValueType[T](using t: Type[T]):
    opaque type Type = T
    given quoted.Type[Type] = t

//  final class PackedValueType[T](using t: Type[T]):
//    opaque type Type1 = T
//    opaque type Type2 = T
//    opaque type Type3 = T
//    opaque type Type4 = T
//    given Type[Type1] = t
//    given Type[Type2] = t
//    given Type[Type3] = t
//    given Type[Type4] = t

  extension (tpe: TypeRepr)
    def asPackedValueType: PackedValueType[?] = tpe.asType match
      case t: Type[Any] @unchecked if tpe <:< TypeRepr.of[Any] => PackedValueType(using t)
      case _ => throw IllegalArgumentException(s"${tpe.safeShow} cannot be used as a value type")

  extension (pos: Position)
    def startPosition = if pos.startLine != pos.endLine then Position(pos.sourceFile, pos.start, pos.start) else pos
    def endPosition = if pos.startLine != pos.endLine then Position(pos.sourceFile, pos.end, pos.end) else pos

  extension (tree: Term | TypeTree | TypeBoundsTree)
    def tpe = tree match
      case tree: Term => tree.tpe
      case tree: TypeTree => tree.tpe
      case tree: TypeBoundsTree => tree.tpe

  extension (symbol: Symbol)
    def findAncestor(predicate: Symbol => Boolean): Option[Symbol] =
      if symbol.exists then
        if predicate(symbol) then Some(symbol) else symbol.maybeOwner.findAncestor(predicate)
      else
        None
    def hasAncestor(predicate: Symbol => Boolean): Boolean =
      findAncestor(predicate).isDefined
    def hasAncestor(ancestors: Symbol*): Boolean =
      symbol hasAncestor { ancestors contains _ }
    def orElse(other: Symbol): Symbol =
      if symbol.exists then symbol else other

  def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol) =
    val symbol = Symbol.newMethod(parent, name, tpe, Flags.EmptyFlags, privateWithin)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, flags.cleaned)
    symbol

  def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol) =
    val symbol = Symbol.newVal(parent, name, tpe, Flags.EmptyFlags, privateWithin)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, flags.cleaned)
    symbol

  def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr) =
    val symbol = Symbol.newBind(parent, name, Flags.EmptyFlags, tpe)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol,  flags.cleaned)
    symbol

  def newClass(parent: Symbol, name: String, flags: Flags, parents: List[TypeRepr], decls: Symbol => List[Symbol], selfType: Option[TypeRepr]) =
    val symbol = Symbol.newClass(parent, name, parents, decls, selfType)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, flags.cleaned)
    symbol

  def newModule(parent: Symbol, name: String, modFlags: Flags, clsFlags: Flags, parents: List[TypeRepr], decls: Symbol => List[Symbol], privateWithin: Symbol) =
    val symbol = Symbol.newModule(parent, name, Flags.EmptyFlags, Flags.EmptyFlags, parents, decls, privateWithin)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, modFlags.cleaned)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol.moduleClass, clsFlags.cleaned)
    symbol

  given ValOrDefDef: TypeTest[Tree, ValDef | DefDef] = tree =>
    summon[TypeTest[Tree, ValDef]].unapply(tree) orElse
    summon[TypeTest[Tree, DefDef]].unapply(tree)

  def contextMethodType[T: Type, R: Type] =
    val Inlined(_, _, Block(List(lambda), _)) = '{ (_: T) ?=> ?[R] }.asTerm: @unchecked
    val tpe @ MethodType(_, _, _) = lambda.symbol.info: @unchecked
    tpe

  def multitierModuleArgument(symbol: Symbol): Option[Term] =
    (symbol.getAnnotation(symbols.`language.multitier`) collect { case Apply(Apply(_, List(arg)), List(_)) => arg }) orElse
    (symbol.getAnnotation(symbols.`embedding.multitier`) collect { case Apply(_, List(arg)) => arg })

  def isMultitierModule(symbol: Symbol): Boolean =
    symbol.exists && (symbol.hasAnnotation(symbols.`language.multitier`) || symbol.hasAnnotation(symbols.`embedding.multitier`))

  def isMultitierNestedPath(symbol: Symbol): Boolean =
    symbol.exists && (isMultitierModule(symbol) || symbol.isModuleDef && isMultitierNestedPath(symbol.maybeOwner))

  def isStablePath(term: Term): Boolean = term match
    case This(_) | Ident(_) => true
    case Select(qualifier, _) => term.symbol.isStable && isStablePath(qualifier)
    case _ => false

  def clearTypeApplications(term: Term): Term = term match
    case Apply(fun, args) =>
      Apply.copy(term)(clearTypeApplications(fun), args)
    case TypeApply(fun, args) => fun.tpe.widenTermRefByName match
      case tpe @ PolyType(_, paramTypes, _) if paramTypes.sizeIs == args.size =>
        TypeApply.copy(term)(clearTypeApplications(fun), (paramTypes.indices map { i => TypeTree.of(using tpe.param(i).asType) }).toList)
      case _ =>
        TypeApply.copy(term)(clearTypeApplications(fun), args)
    case _ =>
      term

  def constructFullName(symbol: Symbol, name: Symbol => String, separator: Symbol => String, skip: Symbol => Boolean): String =
    def constructFullName(symbol: Symbol, suffix: String): String =
      val current = if symbol.isClassDef && symbol.isModuleDef then symbol.companionModule else symbol
      val owner = current.maybeOwner
      val currentName = name(current)

      if owner.exists && suffix.nonEmpty && skip(current) then
        constructFullName(owner, suffix)
      else
        val prefix = if !owner.exists || owner == defn.RootClass then currentName else constructFullName(owner, currentName)

        if prefix.isEmpty || (prefix == "_root_" && suffix.nonEmpty) then
          suffix
        else if suffix.isEmpty then
          prefix
        else
          s"$prefix${separator(current)}$suffix"
    end constructFullName

    constructFullName(symbol, "")
  end constructFullName

  def fullName(symbol: Symbol): String =
    constructFullName(symbol,
      name = _.name,
      separator = symbol => if symbol.isType && !symbol.isPackageDef && !symbol.isModuleDef then "#" else ".",
      skip = symbol => symbol.isAnonymousClass || symbol.isAnonymousFunction || symbol.isPackageObject)
end Commons
