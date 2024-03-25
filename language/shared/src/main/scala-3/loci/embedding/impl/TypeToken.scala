package loci
package embedding
package impl

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.quoted.*
import scala.util.control.Breaks.{break, tryBreakable}

@experimental
case class TypeToken private (token: String, escaped: String)

@experimental
object TypeToken:
  private object token:
    inline def apply(token: String) =
      TypeToken(token, token)
    inline def unapply(token: TypeToken): Option[String] =
      Option.when(token.token == token.escaped) { token.token }

  private inline def tokens(inline tokens: String*) =
    tokens.toList map { token(_) }

  private inline def isEscapedCharacter(ch: Char) =
    !Character.isJavaIdentifierPart(ch)

  private inline def isEscapedInitialCharacter(ch: Char) =
    isEscapedCharacter(ch) || Character.isDigit(ch)

  private inline def isTokenCharacter(ch: Char) =
    ch == '\"' || ch == '\'' || !isEscapedCharacter(ch)

  extension (inline option: Option.type)
    private inline def ensure[T](inline cond: Boolean)(inline body: Option[T]) =
      if cond then body else None

  private transparent inline def isSpecialToken(string: String) = string match
    case "val" | "def" | "this" | "type" | "null" | "true" | "false" => true
    case _ => false

  private def escape(token: String) = token match
    case "val" => TypeToken("val", "\\val")
    case "def" => TypeToken("def", "\\def")
    case "this" => TypeToken("this", "\\this")
    case "type" => TypeToken("type", "\\type")
    case "null" => TypeToken("null", "\\null")
    case "true" => TypeToken("true", "\\true")
    case "false" => TypeToken("false", "\\false")
    case _ =>
      val escaped = token flatMap { ch => if isEscapedCharacter(ch) then s"\\$ch" else ch.toString }
      if token.nonEmpty && isEscapedInitialCharacter(token.head) && !isEscapedCharacter(token.head) then
        TypeToken(token, s"\\$escaped")
      else
        TypeToken(token, escaped)

  def apply(token: String): TypeToken = escape(token)

  def apply(tokens: String*): List[TypeToken] = tokens.toList map escape

  def apply(tokens: List[String]): List[TypeToken] = tokens map escape

  val ` ` = token(" ")
  val `.` = token(".")
  val `#` = token("#")
  val `(` = token("(")
  val `)` = token(")")
  val `[` = token("[")
  val `]` = token("]")
  val `{` = token("{")
  val `}` = token("}")
  val `<` = token("<")
  val `>` = token(">")
  val `:` = tokens(":", " ")
  val `,` = tokens(",", " ")
  val `;` = tokens(";", " ")
  val `&` = tokens(" ", "&", " ")
  val `|` = tokens(" ", "|", " ")

  def serialize(tokens: List[TypeToken]): String =
    (tokens flatMap { _.escaped }).mkString

  def serializeTypeSignature(using Quotes)(tpe: quotes.reflect.TypeRepr): String =
    serialize(typeSignature(tpe))

  def typeSignature(using Quotes)(tpe: quotes.reflect.TypeRepr): List[TypeToken] =
    import quotes.reflect.*

    object Tuple extends TupleExtractor(quotes)

    def tuple(size: Int) = tokens("scala", ".", s"Tuple$size")
    val any = tokens("scala", ".", "Any")

    def underlying(tpe: TypeRepr): TypeRepr = tpe match
      case tpe: AppliedType => underlying(tpe.tycon)
      case tpe: AnnotatedType => underlying(tpe.underlying)
      case tpe: Refinement => underlying(tpe.parent)
      case tpe: ByNameType => underlying(tpe.underlying)
      case _ => tpe

    def symbolSignature(symbol: Symbol): List[TypeToken] =
      val name = (if symbol.isClassDef && symbol.isModuleDef then symbol.companionModule else symbol).name
      if symbol.isPackageObject then
        symbolSignature(symbol.owner)
      else if symbol.maybeOwner == defn.RootClass then
        List(escape(name))
      else if symbol.maybeOwner.isClassDef && !symbol.maybeOwner.isPackageDef && !symbol.maybeOwner.isModuleDef then
        symbolSignature(symbol.maybeOwner) ++ List(`#`, escape(name))
      else if symbol.maybeOwner.exists then
        symbolSignature(symbol.maybeOwner) ++ List(`.`, escape(name))
      else
        List(escape(name))

    def typeSignature(tpe: TypeRepr): List[TypeToken] =
      underlying(tpe.dealiasNonOpaque) match
        case _: AndType | _: OrType if tpe.typeSymbol.exists =>
          symbolSignature(tpe.typeSymbol)
        case Tuple(elements) =>
          tuple(elements.size)
        case tpe: ByNameType =>
          typeSignature(tpe.underlying)
        case tpe: TermRef if tpe.termSymbol.isPackageObject && tpe.qualifier =:= tpe.termSymbol.owner.typeRef =>
          typeSignature(tpe.qualifier)
        case tpe: TypeRef =>
          tpe.qualifier.resolvedTypeMemberType(tpe.name).fold(any) { typeSignature }
        case tpe: NamedType =>
          val symbol = tpe.typeSymbol
          if symbol.exists then
            val prefix = typeSignature(tpe.qualifier)
            val suffix = escape(if symbol.isClassDef && symbol.isModuleDef then symbol.companionModule.name else tpe.name)
            tpe.qualifier match
              case _ if tpe.qualifier.typeSymbol == defn.RootClass =>
                List(suffix)
              case NoPrefix() =>
                List(suffix)
              case TermRef(_, _) =>
                prefix ++ List(`.`, suffix)
              case _ if tpe.qualifier.typeSymbol.isModuleDef || tpe.qualifier.typeSymbol.isPackageDef =>
                prefix ++ List(`.`, suffix)
              case _ =>
                prefix ++ List(`#`, suffix)
          else
            any
        case tpe if tpe.typeSymbol.exists =>
          symbolSignature(tpe.typeSymbol)
        case tpe: TypeBounds =>
          typeSignature(tpe.hi)
        case tpe: AndType =>
          val left = typeSignature(tpe.left)
          val right = typeSignature(tpe.right)
          if left == any then
            right
          else if right == any then
            left
          else
            val leftTokens =
              if tpe.left.typeSymbol.exists then
                left
              else underlying(tpe.left.dealiasNonOpaque) match
                case _: OrType => (TypeToken.`(` :: left) :+ TypeToken.`)`
                case _ => left
            val rightTokens =
              if tpe.right.typeSymbol.exists then
                right
              else underlying(tpe.right.dealiasNonOpaque) match
                case _: OrType => (TypeToken.`(` :: right) :+ TypeToken.`)`
                case _ => right
            leftTokens ++ TypeToken.`&` ++ rightTokens
        case tpe: OrType =>
          val left = typeSignature(tpe.left)
          val right = typeSignature(tpe.right)
          if left == any || right == any then any
          else left ++ TypeToken.`|` ++ right
        case ConstantType(BooleanConstant(value)) =>
          tokens("scala", ".", "Boolean")
        case ConstantType(ByteConstant(value)) =>
          tokens("scala", ".", "Byte")
        case ConstantType(ShortConstant(value)) =>
          tokens("scala", ".", "Short")
        case ConstantType(IntConstant(value)) =>
          tokens("scala", ".", "Int")
        case ConstantType(LongConstant(value)) =>
          tokens("scala", ".", "Long")
        case ConstantType(FloatConstant(value)) =>
          tokens("scala", ".", "Float")
        case ConstantType(DoubleConstant(value)) =>
          tokens("scala", ".", "Double")
        case ConstantType(CharConstant(value)) =>
          tokens("scala", ".", "Char")
        case ConstantType(StringConstant(value)) =>
          tokens("scala", ".", "Predef", ".", "String")
        case ConstantType(UnitConstant()) =>
          tokens("scala", ".", "Unit")
        case ConstantType(NullConstant()) =>
          tokens("scala", ".", "Null")
        case _ =>
          any

    def potentialMethodTypeSignature(tpe: TypeRepr, isMethodType: Boolean): List[TypeToken] = tpe match
      case tpe: ByNameType =>
        if isMethodType then
          typeSignature(tpe.underlying)
        else
          `:` ++  typeSignature(tpe.underlying)

      case tpe: PolyType =>
        tpe.resType match
          case _: MethodOrPoly => potentialMethodTypeSignature(tpe.resType, isMethodType = true)
          case _ => `:` ++ typeSignature(tpe.resType)

      case tpe: MethodType =>
        val separator = tpe.resType match
          case _: MethodOrPoly => List.empty
          case _ => `:`

        val params = tpe.paramTypes.foldRight(List.empty[TypeToken]): (tpe, params) =>
          val param = typeSignature(tpe)
          if params.isEmpty then param else param ++ (`,` ++ params)

        val resType = potentialMethodTypeSignature(tpe.resType, isMethodType = true)

        `(` :: (params :+ `)`) ++ separator ++ resType

      case _ =>
        typeSignature(tpe)
    end potentialMethodTypeSignature

    potentialMethodTypeSignature(tpe, isMethodType = false)
  end typeSignature

  inline def serializeType(using Quotes)(
    inline `tpe | (tpe, from)`: quotes.reflect.TypeRepr | (quotes.reflect.TypeRepr, quotes.reflect.Symbol)): Option[String] =
    inline `tpe | (tpe, from)` match
      case tpe: quotes.reflect.TypeRepr => serializeTypeImpl(tpe, quotes.reflect.defn.RootClass)
      case (tpe: quotes.reflect.TypeRepr, from: quotes.reflect.Symbol) => serializeTypeImpl(tpe, from)

  private def serializeTypeImpl(using Quotes)(tpe: quotes.reflect.TypeRepr, from: quotes.reflect.Symbol): Option[String] =
    fromType(tpe, from) map serialize

  inline def fromType(using Quotes)(
    inline `tpe | (tpe, from)`: quotes.reflect.TypeRepr | (quotes.reflect.TypeRepr, quotes.reflect.Symbol)): Option[List[TypeToken]] =
    inline `tpe | (tpe, from)` match
      case tpe: quotes.reflect.TypeRepr => fromTypeImpl(tpe, quotes.reflect.defn.RootClass)
      case (tpe: quotes.reflect.TypeRepr, from: quotes.reflect.Symbol) => fromTypeImpl(tpe, from)

  private def fromTypeImpl(using Quotes)(tpe: quotes.reflect.TypeRepr, from: quotes.reflect.Symbol): Option[List[TypeToken]] =
    import quotes.reflect.*

    object Tuple extends TupleExtractor(quotes)

    def globallyReachablePrefix(symbol: Symbol): Boolean =
      symbol.exists &&
        (symbol == defn.RootClass ||
          (globallyReachablePrefix(symbol.maybeOwner) &&
            (symbol.isModuleDef || symbol.isPackageDef)))

    def serializeSymbol(symbol: Symbol): Option[List[TypeToken]] =
      Option.ensure(symbol.exists):
        val name = (if symbol.isClassDef && symbol.isModuleDef then symbol.companionModule else symbol).name
        if symbol.isPackageObject then
          serializeSymbol(symbol.owner)
        else if symbol.maybeOwner == defn.RootClass then
          Some(List(escape(name)))
        else if symbol.maybeOwner.isPackageDef || symbol.maybeOwner.isModuleDef then
          serializeSymbol(symbol.maybeOwner) map { _ ++ List(`.`, escape(name)) }
        else if symbol.maybeOwner.isClassDef then
          serializeSymbol(symbol.maybeOwner) map { _ ++ List(`#`, escape(name)) }
        else
          None

    def serializeOuterChain(from: Symbol, to: Symbol): Option[List[TypeToken]] =
      Option.ensure(from.exists && from != defn.RootClass):
        if from == to then
          Some(List(token("!")))
        else if from.maybeOwner.isClassDef && from.maybeOwner != defn.RootClass then
          serializeOuterChain(from.maybeOwner, to) map { token("!") :: _ }
        else
          serializeOuterChain(from.maybeOwner, to)

    def serializeType(
        tpe: TypeRepr,
        binders: Set[LambdaType],
        path: Boolean,
        pathPrefix: Boolean): Option[List[TypeToken]] = tpe match

      case Tuple(elements) if elements.sizeIs > 1 =>
        val tuple = elements.foldRight(Option(List.empty[TypeToken])): (tpe, elements) =>
          serializeType(tpe, binders, path = false, pathPrefix = false) flatMap: tpe =>
            elements map: elements =>
              if elements.isEmpty then tpe :+ `)` else tpe ++ (`,` ++ elements)
        tuple map: tuple =>
          `(` :: tuple
      case tpe: ByNameType =>
        serializeType(tpe.underlying, binders, path, pathPrefix) map:
          tokens("=>", " ") ++ _
      case tpe: TermRef if tpe.termSymbol.isPackageObject && tpe.qualifier =:= tpe.termSymbol.owner.typeRef =>
        serializeType(tpe.qualifier, binders, path, pathPrefix)
      case tpe: NamedType =>
        val symbol = tpe.typeSymbol
        val termName =
          tpe match
            case _: TypeRef if !pathPrefix && symbol.isClassDef && symbol.isModuleDef =>
              Some(symbol.companionModule.name)
            case _: TermRef =>
              Some(tpe.name)
            case _ =>
              None
        val reference =
          tpe.qualifier match
            case TermRef(NoPrefix(), "_root_") | TypeRef(NoPrefix(), "_root_") =>
              Some(List(escape(termName getOrElse tpe.name)))
            case TermRef(_, _) | ThisType(_) | ParamRef(MethodType(_, _, _), _) =>
              serializeType(tpe.qualifier, binders, path = true, pathPrefix = true) map: prefix =>
                if prefix.isEmpty then
                  List(escape(termName getOrElse tpe.name))
                else
                  prefix ++ List(`.`, escape(termName getOrElse tpe.name))
            case _ =>
              serializeType(tpe.qualifier, binders, path = true, pathPrefix = true) map: prefix =>
                prefix ++ List(`#`, escape(termName getOrElse tpe.name))
        termName.fold(reference): _ =>
          if pathPrefix then reference else reference map { _ ++ tokens(".", "type") }
      case tpe: ThisType =>
        val symbol = tpe.tref.typeSymbol
        if symbol == defn.RootClass then
          Some(List.empty)
        else
          val outerChain =
            Option.ensure(from != defn.RootClass && !globallyReachablePrefix(symbol.maybeOwner)):
              serializeOuterChain(from, symbol) map { _ ++ tokens(".", "this") }
          val chain =
            outerChain orElse:
              val chain = serializeSymbol(symbol)
              if symbol.isPackageDef || symbol.isModuleDef then chain else  chain map { _ ++ tokens(".", "this") }
          if pathPrefix then chain else chain map { _ ++ tokens(".", "type") }
      case tpe: AppliedType =>
        val args = tpe.args.foldRight(Option(List.empty[TypeToken])): (tpe, args) =>
          serializeType(tpe, binders, path = false, pathPrefix = false) flatMap: tpe =>
            args map: args =>
              if args.isEmpty then tpe else tpe ++ (`,` ++ args)
        args flatMap: args =>
          serializeType(tpe.tycon, binders, path = true, pathPrefix = false) map:
            _ ++ (`[` :: args) :+ `]`
      case ParamRef(binder: LambdaType, paramNum) =>
        Option.when(binders contains binder):
          token("!") :: escape(binder.paramNames(paramNum)) :: Nil
      case tpe: AnnotatedType =>
        serializeType(tpe.underlying, binders, path, pathPrefix)
      case tpe: Refinement =>
        val nested = tpe.parent match
          case _: Refinement => true
          case _ => false
        val refinement = tpe.info match
          case _: TypeBounds =>
            serializeTypeBounds(tpe.info, binders) map:
              token("type") :: token(" ") :: escape(tpe.name) :: _
          case _: MethodOrPoly | _: ByNameType =>
            serializePotentialMethodType(tpe.info, binders, isMethodType = false) map:
              token("def") :: token(" ") :: escape(tpe.name) :: _
          case _ =>
            serializeType(tpe.info, binders, path = false, pathPrefix = false) map:
              token("val") :: token(" ") :: escape(tpe.name) :: `:` ++ _
        serializeType(tpe.parent, binders, path = !nested, pathPrefix = false) flatMap: parent =>
          refinement map: refinement =>
            val refined =
              if nested then
                parent.dropRight(2) ++ `;` ++ refinement ++ tokens(" ", "}")
              else
                parent ++ tokens(" ", "{", " ") ++ refinement ++ tokens(" ", "}")
            if path then (`(` :: refined) :+ `)` else refined
      case tpe: AndType =>
        serializeType(tpe.left, binders, path = false, pathPrefix = false) flatMap: left =>
          serializeType(tpe.right, binders, path = false, pathPrefix = false) map: right =>
            val leftTokens = tpe.left match
              case _: OrType => (`(` :: left) :+ `)`
              case _ => left
            val rightTokens = tpe.right match
              case _: OrType => (`(` :: right) :+ `)`
              case _ => right
            val conjunction = leftTokens ++ `&` ++ rightTokens
            if path then (`(` :: conjunction) :+ `)` else conjunction
      case tpe: OrType =>
        serializeType(tpe.left, binders, path = false, pathPrefix = false) flatMap: left =>
          serializeType(tpe.right, binders, path = false, pathPrefix = false) map: right =>
            val disjunction = left ++ `|` ++ right
            if path then (`(` :: disjunction) :+ `)` else disjunction
      case tpe: TypeBounds =>
        serializeTypeBounds(tpe, binders) map { token("?") :: _ }
      case ConstantType(BooleanConstant(value)) =>
        Some(tokens(value.toString, ".", "type"))
      case ConstantType(ByteConstant(value)) =>
        Some(tokens(s"${value.toString}b", ".", "type"))
      case ConstantType(ShortConstant(value)) =>
        Some(tokens(s"${value.toString}s", ".", "type"))
      case ConstantType(IntConstant(value)) =>
        Some(tokens(s"${value.toString}i", ".", "type"))
      case ConstantType(LongConstant(value)) =>
        Some(tokens(s"${value.toString}l", ".", "type"))
      case ConstantType(FloatConstant(value)) =>
        Some(tokens(s"${value.toString}f", ".", "type"))
      case ConstantType(DoubleConstant(value)) =>
        Some(tokens(s"${value.toString}d", ".", "type"))
      case ConstantType(CharConstant(value)) =>
        val TypeToken(token, escaped) = escape(value.toString)
        Some(TypeToken(s"\'$token\'", s"\'$escaped\'") :: tokens(".", "type"))
      case ConstantType(StringConstant(value)) =>
        val TypeToken(token, escaped) = escape(value)
        Some(TypeToken(s"\"$token\"", s"\"$escaped\"") :: tokens(".", "type"))
      case ConstantType(UnitConstant()) =>
        Some(tokens("()", ".", "type"))
      case ConstantType(NullConstant()) =>
        Some(tokens("null", ".", "type"))
      case _ =>
        None

    def serializeTypeBounds(
        tpe: TypeRepr,
        binders: Set[LambdaType]): Option[List[TypeToken]] = tpe match
      case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass && hi.typeSymbol == defn.AnyClass =>
        Some(List.empty)
      case TypeBounds(low, hi) if low.typeSymbol == defn.NothingClass =>
        serializeType(hi, binders, path = false, pathPrefix = false) map { tokens(" ", "<:", " ") ++ _ }
      case TypeBounds(low, hi) if hi.typeSymbol == defn.AnyClass =>
        serializeType(low, binders, path = false, pathPrefix = false) map { tokens(" ", ">:", " ") ++ _ }
      case TypeBounds(low, hi) =>
        serializeType(low, binders, path = false, pathPrefix = false) flatMap: low =>
          serializeType(hi, binders, path = false, pathPrefix = false) map: hi =>
            tokens(" ", ">:", " ") ++ low ++ tokens(" ", "<:", " ") ++ hi
      case _ =>
        None

    def serializePotentialMethodType(
        tpe: TypeRepr,
        binders: Set[LambdaType],
        isMethodType: Boolean): Option[List[TypeToken]] = tpe match
      case tpe: ByNameType =>
        Option.ensure(!isMethodType):
          serializeType(tpe.underlying, binders, path = false, pathPrefix = false) map: tpe =>
            `:` ++ tpe

      case tpe: MethodOrPoly =>
        val tpeSeparator = tpe.resType match
          case _: MethodOrPoly => List.empty
          case _ => `:`

        val (paramSeparator, open, close) = tpe match
          case _: MethodType => (`:`, `(`, `)`)
          case _: PolyType => (List.empty, `[`, `]`)

        val paramTypes = tpe.paramTypes.foldRight(Option(List.empty[List[TypeToken]])): (tpe, paramTypes) =>
          val serializedType =
            if paramSeparator.nonEmpty then serializeType(tpe, binders, path = false, pathPrefix = false)
            else serializeTypeBounds(tpe, binders)
          serializedType flatMap { tpe => paramTypes map { tpe :: _ } }

        val params = paramTypes flatMap: paramTypes =>
          (tpe.paramNames zip paramTypes).foldRight(Option(List.empty[TypeToken])):
            case ((name, tpe), params) => params map: params =>
              val param = escape(name) :: paramSeparator ++ tpe
              if params.isEmpty then param else param ++ (`,` ++ params)

        params flatMap: params =>
          serializePotentialMethodType(tpe.resType, binders + tpe, isMethodType = true) map: resType =>
            open :: params ++ (close :: tpeSeparator ++ resType)

      case _ =>
        serializeType(tpe, binders, path = false, pathPrefix = false)
    end serializePotentialMethodType

    serializePotentialMethodType(tpe, Set.empty, isMethodType = false)
  end fromTypeImpl

  def deserialize(string: String): List[TypeToken] =
    val length = string.length
    var i = 0

    inline def hasNext =
      i < length
    inline def next() =
      val j = i
      i += 1
      string(j)
    inline def peak() =
      string(i)

    val tokenBuilder = StringBuilder()
    val escapedBuilder = StringBuilder()

    var tokens = List.empty[TypeToken]

    def buildToken() =
      if tokenBuilder.nonEmpty then
        tokens ::= TypeToken(tokenBuilder.toString(), escapedBuilder.toString())
        tokenBuilder.clear()
        escapedBuilder.clear()

    while hasNext do next() match
      case '\\' =>
        escapedBuilder += '\\'
        if hasNext then
          val c = next()
          tokenBuilder += c
          escapedBuilder += c
        else
          tokenBuilder += '\\'
      case '>' if hasNext && peak() == ':' =>
        buildToken()
        next()
        tokens ::= token(">:")
      case '<' if hasNext && peak() == ':' =>
        buildToken()
        next()
        tokens ::= token("<:")
      case '=' if hasNext && peak() == '>' =>
        buildToken()
        next()
        tokens ::= token("=>")
      case '(' if hasNext && peak() == ')' =>
        buildToken()
        next()
        tokens ::= token("()")
      case ch
        if isTokenCharacter(ch) ||
           ch == '.' && hasNext && Character.isDigit(peak()) ||
           ch == '.' && hasNext && peak() == '.' && tokenBuilder.nonEmpty && Character.isDigit(tokenBuilder.last) =>
        tokenBuilder += ch
        escapedBuilder += ch
      case ch =>
        buildToken()
        tokens ::= token(ch.toString)

    buildToken()
    tokens.reverse
  end deserialize

  inline def deserializeType(using Quotes)(
    inline `tokens | (tokens, from)`: String | (String, quotes.reflect.Symbol)): Option[quotes.reflect.TypeRepr] =
    inline `tokens | (tokens, from)` match
      case tokens: String => deserializeTypeImpl(tokens, quotes.reflect.defn.RootClass)
      case (tokens: String, from: quotes.reflect.Symbol) => deserializeTypeImpl(tokens, from)

  private def deserializeTypeImpl(using Quotes)(tokens: String, from: quotes.reflect.Symbol): Option[quotes.reflect.TypeRepr] =
    toType(deserialize(tokens), from)

  inline def toType(using Quotes)(
    inline `tokens | (tokens, from)`: List[TypeToken] | (List[TypeToken], quotes.reflect.Symbol)): Option[quotes.reflect.TypeRepr] =
    inline `tokens | (tokens, from)` match
      case tokens: List[TypeToken] => toTypeImpl(tokens, quotes.reflect.defn.RootClass)
      case (tokens: List[TypeToken], from: quotes.reflect.Symbol) => toTypeImpl(tokens, from)

  private def toTypeImpl(using Quotes)(tokens: List[TypeToken], from: quotes.reflect.Symbol): Option[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    object Tuple extends TupleExtractor(quotes)

    def separateNested(tokens: List[TypeToken]): Option[(List[List[TypeToken]], List[TypeToken])] =
      object RelatedToken:
        def unapply(start: TypeToken) = start match
          case token("(") => Some(TypeToken.tokens(")"), Some(token(",")))
          case token("[") => Some(TypeToken.tokens("]"), Some(token(",")))
          case token("{") => Some(TypeToken.tokens("}"), Some(token(";")))
          case token("&") => Some(TypeToken.tokens("|", "&"), None)
          case token("|") => Some(TypeToken.tokens("|"), None)
          case token(">:") => Some(TypeToken.tokens("<:"), None)
          case _ => None

      val RelatedToken(end, separator) = tokens.head: @unchecked

      var result = List.empty[List[TypeToken]]
      var nesting = List.empty[TypeToken]
      var pointer = tokens.tail

      while pointer.nonEmpty do
        val current = pointer.head
        if nesting.isEmpty && (end contains current) then
          val tail = tokens.head match
            case token("&" | "|") => pointer
            case _ => pointer.tail
          return
            if result.isEmpty then Some(List.empty, tail)
            else Some((result.head.reverse :: result.tail).reverse, tail)
        else if nesting.isEmpty && (separator contains current) then
          result =
            if result.isEmpty then List(List.empty)
            else List.empty :: result.head.reverse :: result.tail
        else
          nesting =
            current match
              case _ if nesting.headOption contains current => nesting.tail
              case RelatedToken(List(end), Some(_)) => end :: nesting
              case _ => nesting
          result =
            if result.isEmpty then List(List(current))
            else (current :: result.head) :: result.tail
        pointer = pointer.tail
      end while

      None
    end separateNested

    val `null.asInstanceOf` = Select.unique(Literal(NullConstant()), "asInstanceOf")

    val prefixPathMarker = token("\\")

    def markPrefixPath(tokens: List[TypeToken]) = tokens match
      case token("." | "#") :: _ => prefixPathMarker :: tokens
      case _ => tokens

    def deserializeOuterChain(from: Symbol, tokens: List[TypeToken]): Option[(Symbol, List[TypeToken])] =
      Option.ensure(from.exists && from != defn.RootClass):
        tokens match
          case token("!") :: token(".") :: token("this") :: token(".") :: token("type") :: tail =>
            Some(from, tail)
          case token("!") :: token(".") :: token("this") :: tail =>
            Some(from, tail)
          case token("!") :: tail if from.maybeOwner.isClassDef && from.maybeOwner != defn.RootClass =>
            deserializeOuterChain(from.maybeOwner, tail)
          case _ =>
            deserializeOuterChain(from.maybeOwner, tokens)

    def deserializeType(
        tokens: List[TypeToken],
        prefix: Option[TypeRepr],
        typeParams: Map[String, TypeRepr],
        termParams: Map[String, TypeRepr],
        lookupParams: Boolean): Option[TypeRepr] =
      tokens match
        case value :: token(".") :: token("type") :: tokens
            if (isEscapedInitialCharacter(value.escaped.head) && value.escaped.head != '\\') || isSpecialToken(value.escaped) =>
          val tpe =
            Option.ensure(prefix.isEmpty && tokens.isEmpty):
              if (value.token startsWith "\"") && (value.token endsWith "\"") then
                Some(ConstantType(StringConstant(value.token.substring(1, value.token.length - 1))))
              else if (value.token startsWith "\'") && (value.token endsWith "\'") then
                Option.when(value.token.length == 3):
                  ConstantType(CharConstant(value.token(1)))
              else
                Option.ensure(value.token == value.escaped):
                  try
                    if value.token == "true" || value.token == "false" then
                      Some(ConstantType(BooleanConstant(value.token.toBoolean)))
                    else if value.token endsWith "b" then
                      Some(ConstantType(ByteConstant(value.token.init.toByte)))
                    else if value.token endsWith "s" then
                      Some(ConstantType(ShortConstant(value.token.init.toShort)))
                    else if value.token endsWith "i" then
                      Some(ConstantType(IntConstant(value.token.init.toInt)))
                    else if value.token endsWith "l" then
                      Some(ConstantType(LongConstant(value.token.init.toLong)))
                    else if value.token endsWith "f" then
                      Some(ConstantType(FloatConstant(value.token.init.toFloat)))
                    else if value.token endsWith "d" then
                      Some(ConstantType(DoubleConstant(value.token.init.toDouble)))
                    else if value.token == "()" then
                      Some(ConstantType(UnitConstant()))
                    else if value.token == "null" then
                      Some(ConstantType(NullConstant()))
                    else
                      None
                  catch case _: NumberFormatException =>
                    None
          end tpe

          tpe flatMap: tpe =>
            deserializeType(markPrefixPath(tokens), Some(tpe), typeParams, termParams, lookupParams = false)

        case token(value @ ("&" | "|")) :: _ =>
          prefix flatMap: prefix =>
            val (suffix, tail) = separateNested(tokens) match
              case Some(List(tokens), tail) => tokens -> tail
              case _ => tokens.tail -> List.empty
            deserializeType(suffix, None, typeParams, termParams, lookupParams = false) flatMap: suffix =>
              val tpe = if value == "&" then AndType(prefix, suffix) else OrType(prefix, suffix)
              deserializeType(markPrefixPath(tail), Some(tpe), typeParams, termParams, lookupParams = false)

        case token("{") :: _ =>
          separateNested(tokens) flatMap: (tokens, tail) =>
            val tpe = tokens.foldLeft(prefix): (prefix, tokens) =>
              prefix flatMap: prefix =>
                tokens match
                  case token("type") :: name :: tokens =>
                    deserializeTypeBounds(tokens, typeParams, termParams) map: tpe =>
                      Refinement(prefix, name.token, tpe)
                  case token("def") :: name :: tokens =>
                    deserializePotentialMethodType(tokens, typeParams, termParams, isMethodType = false) collect:
                      case tpe @ (_: MethodOrPoly | _: ByNameType) =>
                        Refinement(prefix, name.token, tpe)
                  case token("val") :: name :: token(":") :: tokens =>
                    deserializeType(tokens, None, typeParams, termParams, lookupParams = false) map: tpe =>
                      Refinement(prefix, name.token, tpe)
                  case _ =>
                    None
            deserializeType(markPrefixPath(tail), tpe, typeParams, termParams, lookupParams = false)

        case token("(") :: _ =>
          Option.ensure(prefix.isEmpty):
            separateNested(tokens) flatMap:
              case (List(tokens), tail) =>
                deserializeType(tokens, None, typeParams, termParams, lookupParams = false) flatMap: tpe =>
                  deserializeType(markPrefixPath(tail), Some(tpe), typeParams, termParams, lookupParams = false)
              case (tokens @ _ :: _ :: _, tail) =>
                val elements = tokens.foldRight(Option(List.empty[TypeRepr])): (tokens, elements) =>
                  elements flatMap: elements =>
                    deserializeType(tokens, None, typeParams, termParams, lookupParams = false) map { _ :: elements }
                elements flatMap: elements =>
                  deserializeType(markPrefixPath(tail), Some(Tuple(elements)), typeParams, termParams, lookupParams = false)
              case _ =>
                None

        case token("[") :: _ =>
          prefix flatMap: prefix =>
            separateNested(tokens) flatMap: (tokens, tail) =>
              val args = tokens.foldRight(Option(List.empty[TypeRepr])): (tokens, args) =>
                args flatMap: args =>
                  deserializeType(tokens, None, typeParams, termParams, lookupParams = false) map { _ :: args }
              args flatMap: args =>
                deserializeType(markPrefixPath(tail), Some(AppliedType(prefix, args)), typeParams, termParams, lookupParams = false)

        case token("?") :: tokens =>
          Option.ensure(prefix.isEmpty):
            deserializeTypeBounds(tokens, termParams, typeParams)

        case token("!") :: (token("!") | token(".")) :: _ =>
          Option.ensure(prefix.isEmpty && from != defn.RootClass):
            deserializeOuterChain(from, tokens) flatMap: (symbol, tokens) =>
              deserializeType(markPrefixPath(tokens), Some(ThisType(symbol)), typeParams, termParams, lookupParams = false)

        case token("!") :: tokens =>
          Option.ensure(prefix.isEmpty):
            deserializeType(tokens, prefix, typeParams, termParams, lookupParams = true)

        case token("=>") :: tokens =>
          Option.ensure(prefix.isEmpty):
            deserializeType(tokens, prefix, typeParams, termParams, lookupParams = false) map { ByNameType(_) }

        case head :: tokens =>
          val (isType, isThisType, isSingletonType, tail) = tokens match
            case Nil => (true, false, false, Nil)
            case token(".") :: token("this") :: token(".") :: token("type") :: tail => (true, true, false, tail)
            case token(".") :: token("this") :: tail => (true, true, false, tail)
            case token(".") :: token("type") :: tail => (false, false, true, tail)
            case token(".") :: tail => (false, false, false, tail)
            case token("#") :: tail => (true, false, false, tail)
            case rest => (true, false, false, rest)

          Option.ensure((isTokenCharacter(head.escaped.head) || head.escaped.head == '\\') && !isSpecialToken(head.escaped)):
            val tpe =
              if head eq prefixPathMarker then
                prefix
              else if lookupParams then
                Option.ensure(prefix.isEmpty):
                  if isType then typeParams.get(head.token)
                  else termParams.get(head.token)
              else
                val prefixType = prefix getOrElse defn.RootClass.typeRef
                val prefixSymbol = prefixType.typeSymbol

                val tpe =
                  if prefixSymbol == defn.RootClass && head.token == "_root_" then
                    if isType then Some(defn.RootClass.typeRef) else Some(defn.RootClass.companionModule.termRef)
                  else
                    if isType then
                      prefixType.selectMemberType(head.token)
                    else
                      prefixType.selectMemberField(head.token)

                tpe map: tpe =>
                  if prefixSymbol.exists && (prefixSymbol == defn.RootClass || prefixSymbol.maybeOwner == defn.RootClass) then
                    if isType && tpe.typeSymbol.exists then
                      tpe.typeSymbol.typeRef
                    else if !isType && tpe.termSymbol.exists then
                      tpe.termSymbol.termRef
                    else
                      tpe
                  else
                    tpe
            end tpe

            tpe flatMap: tpe =>
              val tokens = if isThisType || isSingletonType then markPrefixPath(tail) else tail
              val prefix = if isThisType then ThisType(tpe.typeSymbol) else tpe
              deserializeType(tokens, Some(prefix), typeParams, termParams, lookupParams = false)

        case _ =>
          prefix
    end deserializeType

    def deserializeTypeBounds(
        tokens: List[TypeToken],
        typeParams: Map[String, TypeRepr],
        termParams: Map[String, TypeRepr]): Option[TypeBounds] =
      tokens match
        case Nil =>
          Some(TypeBounds.empty)
        case token(">:") :: _ =>
          separateNested(tokens) match
            case Some(List(tokens), tail) =>
              deserializeType(tokens, None, typeParams, termParams, lookupParams = false) flatMap: low =>
                deserializeType(tail, None, typeParams, termParams, lookupParams = false) map: hi =>
                  TypeBounds(low, hi)
            case _ =>
              deserializeType(tokens.tail, None, typeParams, termParams, lookupParams = false) map: low =>
                TypeBounds.lower(low)
        case token("<:") :: _ =>
          deserializeType(tokens.tail, None, typeParams, termParams, lookupParams = false) map: hi =>
            TypeBounds.upper(hi)
        case _ =>
          None

    def deserializePotentialMethodType(
        tokens: List[TypeToken],
        typeParams: Map[String, TypeRepr],
        termParams: Map[String, TypeRepr],
        isMethodType: Boolean): Option[TypeRepr] =
      tokens match
        case token(value @ ("(" | "()")) :: tail =>
          val params =
            if value == "()" then
              Some(List.empty, tail)
            else
              separateNested(tokens)

          params flatMap: (paramTokens, tail) =>
            val params = paramTokens.foldRight(Option(List.empty[(String, TypeRepr)])):
              case (name :: token(":") :: tokens, params) =>
                params flatMap: params =>
                  deserializeType(tokens, None, typeParams, termParams, lookupParams = false) map:
                    name.token -> _ :: params
              case _ =>
                None

            params match
              case Some(params) =>
                tryBreakable {
                  val (paramNames, paramTypes) = params.unzip
                  Some(MethodType(paramNames)(
                    tpe =>
                      paramTypes,
                    tpe =>
                      val params = paramNames.zipWithIndex map { (name, index) => name -> tpe.param(index) }
                      deserializePotentialMethodType(tail, typeParams, termParams ++ params.toMap, isMethodType = true) getOrElse break))
                } catchBreak None
              case _ =>
                Option.ensure(!isMethodType):
                  deserializeType(tokens, None, typeParams, termParams, lookupParams = false)

        case token("[") :: _ =>
          separateNested(tokens) flatMap: (paramTokens, tail) =>
            val params = paramTokens.foldRight(Option(List.empty[(String, TypeBounds)])):
              case (name :: tokens, params) =>
                params flatMap: params =>
                  deserializeTypeBounds(tokens, typeParams, termParams) map:
                    name.token -> _ :: params
              case _ =>
                None

            params flatMap: params =>
              tryBreakable {
                val (paramNames, paramTypeBounds) = params.unzip
                Some(PolyType(paramNames)(
                  tpe =>
                    paramTypeBounds,
                  tpe =>
                    val params = paramNames.zipWithIndex map { (name, index) => name -> tpe.param(index) }
                    deserializePotentialMethodType(tail, typeParams ++ params.toMap, termParams, isMethodType = true) getOrElse break))
              } catchBreak None

        case token(":") :: tokens =>
          deserializeType(tokens, None, typeParams, termParams, lookupParams = false) map: tpe =>
            if isMethodType then tpe else ByNameType(tpe)

        case _ =>
          Option.ensure(!isMethodType):
            deserializeType(tokens, None, typeParams, termParams, lookupParams = false)
    end deserializePotentialMethodType

    val filtered = tokens filterNot: token =>
      token.token.isEmpty || token.escaped.isEmpty || Character.isWhitespace(token.escaped.head)

    Option.ensure(filtered.nonEmpty):
      deserializePotentialMethodType(filtered, Map.empty, Map.empty, isMethodType = false)
  end toTypeImpl
end TypeToken
