package loci
package embedding
package impl

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.quoted.*
import scala.util.control.Breaks.{break, tryBreakable}

@experimental
case class TypeToken(token: String, escaped: String)

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

  private trait TupleExtractor[Q <: Quotes & Singleton](val quotes: Q):
    import quotes.reflect.*

    private given quotes.type = quotes

    private val nil = TypeRepr.of[EmptyTuple].typeSymbol
    private val cons = TypeRepr.of[? *: ?].typeSymbol

    def apply(elements: List[TypeRepr]): TypeRepr =
      if elements.nonEmpty && elements.sizeIs < 23 then
        defn.TupleClass(elements.size).typeRef.appliedTo(elements)
      else
        elements.foldRight[TypeRepr](nil.typeRef): (tpe, tuple) =>
          cons.typeRef.appliedTo(List(tpe, tuple))

    def unapply(tpe: TypeRepr): Option[List[TypeRepr]] = tpe match
      case _ if tpe.typeSymbol == nil =>
        Some(List.empty)
      case AppliedType(tycon, List(head, tail)) if tycon.typeSymbol == cons =>
        unapply(tail) map { head :: _ }
      case AppliedType(_, elements) if tpe.isTupleN =>
        Some(elements)
      case _ =>
        None
  end TupleExtractor

  def apply(token: String): TypeToken = escape(token)

  def serialize(tokens: List[TypeToken]): String =
    (tokens flatMap { _.escaped }).mkString

  def serializeType(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[String] =
    fromType(tpe) map serialize

  def fromType(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[List[TypeToken]] =
    import quotes.reflect.*

    object Tuple extends TupleExtractor(quotes)

    def serializeSymbol(symbol: Symbol): Option[List[TypeToken]] =
      Option.ensure(symbol.exists):
        val name = (if symbol.isClassDef && symbol.isModuleDef then symbol.companionModule else symbol).name
        if symbol.maybeOwner == defn.RootClass then
          Some(List(escape(name)))
        else if symbol.maybeOwner.isPackageDef || symbol.maybeOwner.isModuleDef then
          serializeSymbol(symbol.maybeOwner) map { _ ++ List(token("."), escape(name)) }
        else if symbol.maybeOwner.isClassDef then
          serializeSymbol(symbol.maybeOwner) map { _ ++ List(token("#"), escape(name)) }
        else
          None

    def serializeType(
        tpe: TypeRepr,
        binders: Set[LambdaType],
        path: Boolean,
        pathPrefix: Boolean): Option[List[TypeToken]] = tpe match
      case Tuple(elements) if elements.sizeIs > 1 =>
        val tuple = elements.foldRight(Option(List.empty[TypeToken])): (tpe, elements) =>
          serializeType(tpe, binders, path = false, pathPrefix = false) flatMap: tpe =>
            elements map: elements =>
              if elements.isEmpty then tpe :+ token(")") else tpe ++ (tokens(",", " ") ++ elements)
        tuple map: tuple =>
          token("(") :: tuple
      case NoPrefix() =>
        Some(List.empty)
      case tpe: NamedType =>
        val reference =
          tpe.qualifier match
            case TermRef(NoPrefix(), "_root_") | TypeRef(NoPrefix(), "_root_") =>
              Some(List(escape(tpe.name)))
            case NoPrefix() | TermRef(_, _) | ThisType(_) | ParamRef(MethodType(_, _, _), _) =>
              serializeType(tpe.qualifier, binders, path = true, pathPrefix = true) map: prefix =>
                if prefix.isEmpty then
                  List(escape(tpe.name))
                else
                  prefix ++ List(token("."), escape(tpe.name))
            case _ =>
              serializeType(tpe.qualifier, binders, path = true, pathPrefix = true) map: prefix =>
                prefix ++ List(token("#"), escape(tpe.name))
        tpe match
          case _: TermRef if !pathPrefix => reference map { _ ++ tokens(".", "type") }
          case _ => reference
      case tpe: ThisType =>
        val symbol = tpe.tref.typeSymbol
        if symbol == defn.RootClass then
          Some(List.empty)
        else if symbol.isPackageDef || symbol.isModuleDef then
          serializeSymbol(symbol)
        else
          serializeSymbol(symbol) map { _ ++ tokens(".", "this") }
      case tpe: AppliedType =>
        val args = tpe.args.foldRight(Option(List.empty[TypeToken])): (tpe, args) =>
          serializeType(tpe, binders, path = false, pathPrefix = false) flatMap: tpe =>
            args map: args =>
              if args.isEmpty then tpe else tpe ++ (tokens(",", " ") ++ args)
        args flatMap: args =>
          serializeType(tpe.tycon, binders, path = true, pathPrefix = false) map:
            _ ++ (token("[") :: args) :+ token("]")
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
              token("val") :: token(" ") :: escape(tpe.name) :: token(":") :: token(" ") :: _
        serializeType(tpe.parent, binders, path = !nested, pathPrefix = false) flatMap: parent =>
          refinement map: refinement =>
            val refined =
              if nested then
                parent.dropRight(2) ++ tokens(";", " ") ++ refinement ++ tokens(" ", "}")
              else
                parent ++ tokens(" ", "{", " ") ++ refinement ++ tokens(" ", "}")
            if path then (token("(") :: refined) :+ token(")") else refined
      case tpe: AndType =>
        serializeType(tpe.left, binders, path = false, pathPrefix = false) flatMap: left =>
          serializeType(tpe.right, binders, path = false, pathPrefix = false) map: right =>
            val leftTokens = tpe.left match
              case _: OrType => (token("(") :: left) :+ token(")")
              case _ => left
            val rightTokens = tpe.right match
              case _: OrType => (token("(") :: right) :+ token(")")
              case _ => right
            val conjunction = leftTokens ++ tokens(" ", "&", " ") ++ rightTokens
            if path then (token("(") :: conjunction) :+ token(")") else conjunction
      case tpe: OrType =>
        serializeType(tpe.left, binders, path = false, pathPrefix = false) flatMap: left =>
          serializeType(tpe.right, binders, path = false, pathPrefix = false) map: right =>
            val disjunction = left ++ tokens(" ", "|", " ") ++ right
            if path then (token("(") :: disjunction) :+ token(")") else disjunction
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
            tokens(":", " ") ++ tpe

      case tpe: MethodOrPoly =>
        val tpeSeparator = tpe.resType match
          case _: MethodOrPoly => List.empty
          case _ => tokens(":", " ")

        val (paramSeparator, open, close) = tpe match
          case _: MethodType => (tokens(":", " "), token("("), List(token(")")) ++ tpeSeparator)
          case _: PolyType => (List.empty, token("["), List(token("]")) ++ tpeSeparator)

        val paramTypes = tpe.paramTypes.foldRight(Option(List.empty[List[TypeToken]])): (tpe, paramTypes) =>
          val serializedType =
            if paramSeparator.nonEmpty then serializeType(tpe, binders, path = false, pathPrefix = false)
            else serializeTypeBounds(tpe, binders)
          serializedType flatMap { tpe => paramTypes map { tpe :: _ } }

        val params = paramTypes flatMap: paramTypes =>
          (tpe.paramNames zip paramTypes).foldRight(Option(List.empty[TypeToken])):
            case ((name, tpe), params) => params map: params =>
              val param = escape(name) :: paramSeparator ++ tpe
              if params.isEmpty then param else param ++ (tokens(",", " ") ++ params)

        params flatMap: params =>
          serializePotentialMethodType(tpe.resType, binders + tpe, isMethodType = true) map: resType =>
            open :: params ++ close ++ resType

      case _ =>
        serializeType(tpe, binders, path = false, pathPrefix = false)
    end serializePotentialMethodType

    serializePotentialMethodType(tpe, Set.empty, isMethodType = false)
  end fromType

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

  def deserializeType(using Quotes)(tokens: String): Option[quotes.reflect.TypeRepr] =
    toType(deserialize(tokens))

  def toType(using Quotes)(tokens: List[TypeToken]): Option[quotes.reflect.TypeRepr] =
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

        case token("!") :: tokens =>
          Option.ensure(prefix.isEmpty):
            deserializeType(tokens, prefix, typeParams, termParams, lookupParams = true)

        case head :: tokens =>
          val (isType, isThisType, isSingletonType, tail) = tokens match
            case Nil => (true, false, false, Nil)
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
              val prefix = if isThisType then This(tpe.typeSymbol).tpe else tpe
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
  end toType
end TypeToken
