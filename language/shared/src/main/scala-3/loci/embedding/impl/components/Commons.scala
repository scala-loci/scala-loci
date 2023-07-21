package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.reflect.TypeTest
import scala.quoted.*

@experimental
trait Commons:
  this: Component =>
  import quotes.reflect.*

  object symbols:
    val `language.Local` = Symbol.requiredPackage("loci.language").typeMember("Local")
    val `language.per` = Symbol.requiredPackage("loci.language").typeMember("per")
    val `language.on` = Symbol.requiredPackage("loci.language").typeMember("on")
    val `embedding.on` = Symbol.requiredPackage("loci.embedding").typeMember("on")
    val on = TypeRepr.of[On[?]].typeSymbol
    val placed = TypeRepr.of[Placed[?, ?]].typeSymbol
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]].typeSymbol
    val remote = TypeRepr.of[language.Remote[?]].typeSymbol
    val multitier = TypeRepr.of[language.multitier].typeSymbol
    val peer = TypeRepr.of[language.peer].typeSymbol
    val single = TypeRepr.of[language.Single[?]].typeSymbol
    val optional = TypeRepr.of[language.Optional[?]].typeSymbol
    val multiple = TypeRepr.of[language.Multiple[?]].typeSymbol
    val function1 = TypeRepr.of[Function1[?, ?]].typeSymbol
    val contextFunction1 = TypeRepr.of[ContextFunction1[?, ?]].typeSymbol
    val contextResultCount = TypeRepr.of[annotation.internal.ContextResultCount].typeSymbol
    val compileTimeOnly = TypeRepr.of[annotation.compileTimeOnly].typeSymbol
    val targetName = TypeRepr.of[annotation.targetName].typeSymbol
    val erased = '{ embedding.erased }.asTerm.underlyingArgument.symbol
    val erasedArgs = '{ embedding.erased(()) }.asTerm.underlyingArgument.symbol
    val asInstanceOf = '{ null.asInstanceOf }.asTerm.underlyingArgument.symbol

  object types:
    val placedValue = TypeRepr.of[PlacedValue[?, ?]]
    val placed = TypeRepr.of[Placed[?, ?]]
    val subjective = TypeRepr.of[Placed.Subjective[?, ?]]
    val remote = TypeRepr.of[language.Remote[?]]
    val context = TypeRepr.of[Placement.Context[?]]
    val conversion = TypeRepr.of[Conversion[?, ?]]
    val placedValues = TypeRepr.of[loci.runtime.PlacedValues]

  object names:
//    val sbj = "sbj"
    val body = "body"
    val apply = "apply"
    val tie = "Tie"

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

  def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol) =
    val symbol = Symbol.newMethod(parent, name, tpe, Flags.EmptyFlags, privateWithin)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, flags &~ Flags.EmptyFlags)
    symbol

  def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol) =
    val symbol = Symbol.newVal(parent, name, tpe, Flags.EmptyFlags, privateWithin)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, flags &~ Flags.EmptyFlags)
    symbol

  def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr) =
    val symbol = Symbol.newBind(parent, name, Flags.EmptyFlags, tpe)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol,  flags &~ Flags.EmptyFlags)
    symbol

  def newClass(parent: Symbol, name: String, flags: Flags, parents: List[TypeRepr], decls: Symbol => List[Symbol], selfType: Option[TypeRepr]) =
    val symbol = Symbol.newClass(parent, name, parents, decls, selfType)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, flags &~ Flags.EmptyFlags)
    symbol

  def newModule(parent: Symbol, name: String, modFlags: Flags, clsFlags: Flags, parents: List[TypeRepr], decls: Symbol => List[Symbol], privateWithin: Symbol) =
    val symbol = Symbol.newModule(parent, name, Flags.EmptyFlags, Flags.EmptyFlags, parents, decls, privateWithin)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol, modFlags &~ Flags.EmptyFlags)
    SymbolMutator.getOrErrorAndAbort.setFlag(symbol.moduleClass, clsFlags &~ Flags.EmptyFlags)
    symbol

  given ValOrDefDef: TypeTest[Tree, ValDef | DefDef] = tree =>
    summon[TypeTest[Tree, ValDef]].unapply(tree) orElse
    summon[TypeTest[Tree, DefDef]].unapply(tree)

  def contextMethodType[T: Type, R: Type] =
    val Inlined(_, _, Block(List(lambda), _)) = '{ (_: T) ?=> erased: R }.asTerm: @unchecked
    val tpe @ MethodType(_, _, _) = lambda.symbol.info: @unchecked
    tpe

  def isMultitierModule(symbol: Symbol): Boolean =
    symbol.getAnnotation(symbols.multitier).isDefined

  def fullName(symbol: Symbol): String =
    def fullName(symbol: Symbol, name: String): String =
      val owner = symbol.maybeOwner

      val symbolName = if symbol.isType && symbol.isModuleDef && (symbol.name endsWith "$") then symbol.name.dropRight(1) else symbol.name
//        if (symbol.name startsWith "$loci$multitier$")
//          (owner.info member TermName(symbol.name.drop(16)) orElse symbol).name.toString
//        else
//          symbol.name

      if owner.exists && ((symbol.flags is Flags.Synthetic) || (symbolName startsWith "<") && (symbolName endsWith ">")) then
        fullName(owner, name)
      else
        val prefix = if !owner.exists || owner == defn.RootClass then symbolName else fullName(owner, symbolName)

        if prefix.isEmpty || (prefix == "_root_" && name.nonEmpty) then
          name
        else if name.isEmpty then
          prefix
        else
          val separator = if symbol.isType && !symbol.isPackageDef && !symbol.isModuleDef then "#" else "."
          s"$prefix$separator$name"
    end fullName

    fullName(symbol, "")
  end fullName
end Commons
