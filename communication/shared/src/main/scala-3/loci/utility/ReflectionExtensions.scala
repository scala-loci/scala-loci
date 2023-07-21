package loci
package utility

import scala.annotation.targetName
import scala.quoted.*
import scala.util.control.NonFatal

object reflectionExtensions:
  extension (using Quotes)(symbol: quotes.reflect.Symbol)
    def isAbstract =
      (symbol.flags is quotes.reflect.Flags.Abstract) ||
      (symbol.flags is quotes.reflect.Flags.Deferred)

    def isMethod =
      symbol.isTerm &&
      !symbol.isClassConstructor &&
      (symbol.flags is quotes.reflect.Flags.Method)

    def isField =
      symbol.isTerm &&
      !(symbol.flags is quotes.reflect.Flags.Method) &&
      !(symbol.flags is quotes.reflect.Flags.Module) &&
      !(symbol.flags is quotes.reflect.Flags.Package)

    def isFieldAccessor =
      symbol.flags is quotes.reflect.Flags.FieldAccessor

    def isCaseAccessor =
      symbol.flags is quotes.reflect.Flags.CaseAccessor

    def isParamAccessor =
      symbol.flags is quotes.reflect.Flags.ParamAccessor

    def isStable =
      symbol.isTerm &&
      ((!(symbol.flags is quotes.reflect.Flags.Mutable) &&
        !(symbol.flags is quotes.reflect.Flags.Method)) ||
       (symbol.flags is quotes.reflect.Flags.StableRealizable))

    def isPublic =
      !(symbol.flags is quotes.reflect.Flags.Protected) &&
      !(symbol.flags is quotes.reflect.Flags.Private) &&
      !(symbol.flags is quotes.reflect.Flags.PrivateLocal) &&
      !(symbol.flags is quotes.reflect.Flags.Local)

    def isImplicit =
      (symbol.flags is quotes.reflect.Flags.Implicit) ||
      (symbol.flags is quotes.reflect.Flags.Given)

    def isExtensionMethod =
      symbol.flags is quotes.reflect.Flags.ExtensionMethod

    def isModuleDef =
      symbol.flags is quotes.reflect.Flags.Module
  end extension

  extension (using Quotes)(flags: quotes.reflect.Flags)
    def &~(other: quotes.reflect.Flags) =
      import quotes.reflect.*
      import Flags.*

      var allFlags = List(
        Abstract, Artifact, Case, CaseAccessor, Contravariant, Covariant, Deferred,
        EmptyFlags, Enum, Erased, Exported, ExtensionMethod, FieldAccessor, Final,
        Given, HasDefault, Implicit, Infix, Inline, Invisible, Lazy, Local, Macro, Method, Module, Mutable,
        NoInits, Opaque, Open, Override, Package, Param, ParamAccessor, Private, PrivateLocal, Protected,
        Scala2x, Sealed, StableRealizable, Synthetic, Trait, Transparent)

      try
        val flagsClass = Class.forName("dotty.tools.dotc.core.Flags$")
        val flags = flagsClass.getField("MODULE$")
        val absOverride = flagsClass.getMethod("AbsOverride")
        absOverride.invoke(flags.get(null)) match
          case flags: Flags @unchecked if Flags.EmptyFlags.getClass.isInstance(flags) => allFlags ::= flags
          case _ =>
      catch
        case _: ClassNotFoundException | _: NoSuchFieldException | _: NoSuchMethodException =>

      allFlags.foldLeft(EmptyFlags) { (result, flag) =>
        if (flags is flag) && !(other is flag) then result | flag else result
      }
  end extension

  extension (using Quotes)(tree: quotes.reflect.Tree)
    @targetName("safeShowTree") def safeShow: String = tree.safeShow("<?>", quotes.reflect.Printer.TreeCode)
    @targetName("safeShowTree") def safeShow(fallback: String): String = tree.safeShow(fallback, quotes.reflect.Printer.TreeCode)
    @targetName("safeShowTree") def safeShow(printer: quotes.reflect.Printer[quotes.reflect.Tree]): String = tree.safeShow("<?>", printer)
    @targetName("safeShowTree") def safeShow(fallback: String, printer: quotes.reflect.Printer[quotes.reflect.Tree]): String =
      try
        val result = tree.show(using printer).trim
        if result.nonEmpty then result else fallback
      catch
        case NonFatal(_) => fallback

    def posInUserCode =
      import quotes.reflect.*

      val splicePos = Position.ofMacroExpansion

      inline def isSynthetic(pos: Position) =
        pos.toString endsWith ">"

      inline def inUserCode(pos: Position) =
        pos.sourceFile == splicePos.sourceFile &&
        pos.start >= splicePos.start &&
        pos.end <= splicePos.end &&
        (pos.start != splicePos.start || (pos.end != splicePos.start && pos.end != splicePos.end)) &&
        !isSynthetic(pos)

      object positionsAccumulator extends TreeAccumulator[List[Position]]:
        def foldTree(positions: List[Position], tree: Tree)(owner: Symbol) =
          if inUserCode(tree.pos) then
            foldOverTree(tree.pos :: positions, tree)(owner)
          else
            foldOverTree(positions, tree)(owner)

      if inUserCode(tree.pos) then
        tree.pos
      else
        val positions = positionsAccumulator.foldTree(List.empty, tree)(Symbol.noSymbol)
        if positions.nonEmpty then
          positions reduce { (pos0, pos1) =>
            Position(splicePos.sourceFile, math.min(pos0.start, pos1.start), math.max(pos0.end, pos1.end))
          }
        else
          Position.ofMacroExpansion
    end posInUserCode
  end extension

  extension (using Quotes)(term: quotes.reflect.Term)
    def substituteRefs(using Quotes)(
        from: quotes.reflect.Symbol,
        to: quotes.reflect.Symbol,
        owner: quotes.reflect.Symbol) =
      Substitutor.substituteRefsInTerm(Map(from -> to), owner, term)

    def substituteRefs(using Quotes)(
        substitutions: Map[quotes.reflect.Symbol, quotes.reflect.Symbol],
        owner: quotes.reflect.Symbol) =
      Substitutor.substituteRefsInTerm(substitutions, owner, term)
  end extension

  extension (using Quotes)(tpe: quotes.reflect.TypeRepr)
    @targetName("safeShowType") def safeShow: String = tpe.safeShow("<?>", quotes.reflect.Printer.SafeTypeReprCode)
    @targetName("safeShowType") def safeShow(fallback: String): String = tpe.safeShow(fallback, quotes.reflect.Printer.SafeTypeReprCode)
    @targetName("safeShowType") def safeShow(printer: quotes.reflect.Printer[quotes.reflect.TypeRepr]): String = tpe.safeShow("<?>", printer)
    @targetName("safeShowType") def safeShow(fallback: String, printer: quotes.reflect.Printer[quotes.reflect.TypeRepr]): String =
      try
        val result = tpe.show(using printer).trim
        if result.nonEmpty then result else fallback
      catch
        case NonFatal(_) => fallback

    def typeConstructor: quotes.reflect.TypeRepr =
      tpe match
        case quotes.reflect.AppliedType(tycon, _) => tycon
        case _ => tpe

    def typeArgs: List[quotes.reflect.TypeRepr] =
      tpe match
        case quotes.reflect.AppliedType(_, args) => args
        case _ => List.empty

    def typeArgVariances: List[quotes.reflect.Flags] =
      tpe match
        case TypeArgVariances(variances) => variances
        case _ => List.empty

    def resultType: quotes.reflect.TypeRepr =
      tpe match
        case tpe: quotes.reflect.MethodOrPoly => tpe.resType.resultType
        case tpe: quotes.reflect.ByNameType => tpe.underlying.resultType
        case _ => tpe

    def withResultType(res: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*
      tpe match
        case MethodType(paramNames, paramTypes, resType) =>
          MethodType(paramNames)(_ => paramTypes, _ => resType.withResultType(res))
        case PolyType(paramNames, paramBounds, resType) =>
          MethodType(paramNames)(_ => paramBounds, _ => resType.withResultType(res))
        case ByNameType(underlying) =>
          ByNameType(underlying.withResultType(res))
        case _ =>
          res

    def contextFunctionResultType: quotes.reflect.TypeRepr =
      if tpe.isContextFunctionType then
        tpe.widenDealias.typeArgs.lastOption map { _.contextFunctionResultType } getOrElse tpe
      else
        tpe

    def finalResultType: quotes.reflect.TypeRepr =
      tpe.resultType.contextFunctionResultType

    def widenDealias: quotes.reflect.TypeRepr =
      val dealiased = tpe.dealias
      if tpe != dealiased then
        dealiased.widenDealias
      else
        val widened = tpe.widen
        if tpe != widened then
          widened.widenDealias
        else
          tpe

    def stripLazyRef: quotes.reflect.TypeRepr =
      LazyRefStripping.strip(tpe)

    def substitute(from: quotes.reflect.ParamRef, to: quotes.reflect.TypeRepr) =
      TypeParamSubstition.substitute(tpe, from, to)

    def substituteRefs(using Quotes)(from: quotes.reflect.Symbol, to: quotes.reflect.Symbol) =
      Substitutor.substituteRefsInType(Map(from -> to), tpe)

    def substituteRefs(using Quotes)(substitutions: Map[quotes.reflect.Symbol, quotes.reflect.Symbol]) =
      Substitutor.substituteRefsInType(substitutions, tpe)

    def substituteParamRefs(
        fromBinder: quotes.reflect.LambdaType | quotes.reflect.RecursiveType,
        toBinder: quotes.reflect.LambdaType | quotes.reflect.RecursiveType) =
      Substitutor.substituteParamRefsInType(Map(fromBinder -> toBinder), tpe)

    def substituteParamRefs(substitutionBinders: Map[
        quotes.reflect.LambdaType | quotes.reflect.RecursiveType,
        quotes.reflect.LambdaType | quotes.reflect.RecursiveType]) =
      Substitutor.substituteParamRefsInType(substitutionBinders, tpe)

    def resolvedMemberType(symbol: quotes.reflect.Symbol) =
      import quotes.reflect.*

      def memberTypeInRefinement(tpe: TypeRepr, name: String): Option[TypeRepr] = tpe match
        case Refinement(_, `name`, info) => Some(info)
        case Refinement(parent, _, _) => memberTypeInRefinement(parent, name)
        case _ => None

      memberTypeInRefinement(tpe, symbol.name) orElse
      memberTypeInRefinement(tpe.dealias, symbol.name) getOrElse
      tpe.memberType(tpe.baseClasses
        collectFirst Function.unlift { base =>
          val overriding = symbol.overridingSymbol(base)
          Option.when(overriding.exists)(overriding)
        }
        getOrElse symbol)
  end extension

  private object Substitutor:
    def substituteRefsInTerm(using Quotes)(
        substitutions: Map[quotes.reflect.Symbol, quotes.reflect.Symbol],
        owner: quotes.reflect.Symbol,
        term: quotes.reflect.Term): quotes.reflect.Term =
      import quotes.reflect.*

      object substitutor extends SafeTreeMap(quotes):
        override def transformTerm(term: Term)(owner: Symbol) = term match
          case term: This =>
            substitutions.get(term.symbol).fold(super.transformTerm(term)(owner)) { This(_) }
          case term: Ident =>
            substitutions.get(term.symbol).fold(super.transformTerm(term)(owner)) { Ref(_) }
          case term: Select =>
            substitutions.get(term.symbol).fold(super.transformTerm(term)(owner)) { symbol =>
              transformTerm(term.qualifier)(owner).select(symbol)
            }
          case _ =>
            super.transformTerm(term)(owner)

        override def transformTypeTree(tree: TypeTree)(owner: Symbol) = tree match
          case Inferred() =>
            val tpe = substituteRefsInType(substitutions, tree.tpe)
            if tpe != tree.tpe then TypeTree.of(using tpe.asType) else tree
          case tree: TypeIdent =>
            substitutions.get(tree.symbol).fold(super.transformTypeTree(tree)(owner)) { TypeIdent(_) }
          case tree: TypeSelect =>
            substitutions.get(tree.symbol).fold(super.transformTypeTree(tree)(owner)) { symbol =>
              TypeSelect(transformTerm(tree.qualifier)(owner), symbol.name)
            }
          case _ =>
            super.transformTypeTree(tree)(owner)
      end substitutor

      substitutor.transformTerm(term)(owner)
    end substituteRefsInTerm

    def substituteRefsInType(using Quotes)(
        substitutions: Map[quotes.reflect.Symbol, quotes.reflect.Symbol],
        tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*

      object substitutor extends TypeMap(quotes):
        override def transform(tpe: TypeRepr) = tpe match
          case tpe: NamedType =>
            substitutions.get(tpe.typeSymbol).fold(super.transform(tpe)) { transform(tpe.qualifier).select(_) }
          case tpe =>
            super.transform(tpe)

      substitutor.transform(tpe)
    end substituteRefsInType

    def substituteParamRefsInType(using Quotes)(
        substitutionBinders: Map[
          quotes.reflect.LambdaType | quotes.reflect.RecursiveType,
          quotes.reflect.LambdaType | quotes.reflect.RecursiveType],
        tpe: quotes.reflect.TypeRepr): quotes.reflect.TypeRepr =
      import quotes.reflect.*

      val substitutions = substitutionBinders.toMap[TypeRepr, LambdaType | RecursiveType]

      object substitutor extends TypeMap(quotes):
        override def transform(tpe: TypeRepr) = tpe match
          case ParamRef(binder, paramNum) =>
            substitutions.get(binder).fold(tpe) {
              case binder: MethodType => binder.param(paramNum)
              case binder: PolyType => binder.param(paramNum)
              case binder: TypeLambda => binder.param(paramNum)
              case _ => tpe
            }
          case RecursiveThis(binder) =>
            substitutions.get(binder).fold(tpe) {
              case binder: RecursiveType => binder.recThis
              case _ => tpe
            }
          case tpe =>
            super.transform(tpe)

      substitutor.transform(tpe)
    end substituteParamRefsInType
  end Substitutor

  trait TypeMap[Q <: Quotes & Singleton](val quotes: Q):
    import quotes.reflect.*

    private given quotes.type = quotes

    def transform(tpe: TypeRepr): TypeRepr = tpe match
      case tpe: NamedType =>
        def member(symbol: Symbol, name: String) = tpe match
          case _: TypeRef => symbol.typeMember(name)
          case _: TermRef => symbol.fieldMember(name)
        val qualifier = transform(tpe.qualifier)
        if qualifier != tpe.qualifier then
          val symbol = member(qualifier.typeSymbol, tpe.name)
          val memberSymbol = if symbol.exists then symbol else member(qualifier.termSymbol, tpe.name)
          qualifier.select(memberSymbol)
        else
          tpe
      case tpe: ThisType =>
        val tref = transform(tpe.tref)
        if tref != tpe.tref then This(tref.typeSymbol).tpe else tpe
      case SuperType(tpethis, tpesuper) =>
        val thistpe = transform(tpethis)
        val supertpe = transform(tpesuper)
        if thistpe != tpethis || supertpe != tpesuper then SuperType(thistpe, supertpe) else tpe
      case tpe: AppliedType =>
        val tycon = transform(tpe.tycon)
        val args = tpe.args map transform
        if tycon != tpe.tycon || args != tpe.args then AppliedType(tycon, args) else tpe
      case tpe: TypeBounds =>
        val low = transform(tpe.low)
        val hi = transform(tpe.hi)
        if low != tpe.low || hi != tpe.hi then TypeBounds(low, hi) else tpe
      case tpe: Refinement =>
        val parent = transform(tpe.parent)
        val info = transform(tpe.info)
        if parent != tpe.parent || info != tpe.info then Refinement(parent, tpe.name, info) else tpe
      case tpe: MatchType =>
        val bound = transform(tpe.bound)
        val scrutinee = transform(tpe.scrutinee)
        val cases = tpe.cases map transform
        if bound != tpe.bound || scrutinee != tpe.scrutinee || cases != tpe.cases then MatchType(bound, scrutinee, cases) else tpe
      case tpe: MatchCase =>
        val pattern = transform(tpe.pattern)
        val rhs = transform(tpe.rhs)
        if pattern != tpe.pattern || rhs != tpe.rhs then MatchCase(pattern, rhs) else tpe
      case tpe: ByNameType =>
        val underlying = transform(tpe.underlying)
        if underlying != tpe.underlying then ByNameType(underlying) else tpe
      case tpe: MethodType =>
        val paramTypes = tpe.paramTypes map transform
        val resType = transform(tpe.resType)
        if paramTypes != tpe.paramTypes || resType != tpe.resType then
          val methodType = MethodType(tpe.paramNames)(_ => paramTypes, _ => resType)
          methodType.substituteParamRefs(tpe, methodType)
        else
          tpe
      case tpe: PolyType =>
        val paramBounds = tpe.paramBounds map { transform(_) match
          case tpe: TypeBounds => tpe
          case _ => TypeBounds.empty
        }
        val resType = transform(tpe.resType)
        if paramBounds != tpe.paramBounds || resType != tpe.resType then
          val polyType = PolyType(tpe.paramNames)(_ => paramBounds, _ => resType)
          polyType.substituteParamRefs(tpe, polyType)
        else
          tpe
      case tpe: TypeLambda =>
        val paramTypes = tpe.paramTypes map { transform(_) match
          case tpe: TypeBounds => tpe
          case _ => TypeBounds.empty
        }
        val resType = transform(tpe.resType)
        if paramTypes != tpe.paramTypes || resType != tpe.resType then
          val typeLambda = TypeLambda(tpe.paramNames, _ => paramTypes, _ => resType)
          typeLambda.substituteParamRefs(tpe, typeLambda)
        else
          tpe
      case tpe: RecursiveType =>
        val underlying = transform(tpe.underlying)
        if underlying != tpe.underlying then
          val recursiveType = RecursiveType(_ => underlying)
          recursiveType.substituteParamRefs(tpe, recursiveType)
        else
          tpe
      case _ =>
        tpe
  end TypeMap

  trait SafeTreeMap[Q <: Quotes & Singleton](val quotes: Q):
    import quotes.reflect.*

    private given quotes.type = quotes

    private object underlying extends TreeMap:
      override def transformTree(tree: Tree)(owner: Symbol): Tree =
        SafeTreeMap.this.transformTree(tree)(owner)
      override def transformStatement(tree: Statement)(owner: Symbol): Statement =
        SafeTreeMap.this.transformStatement(tree)(owner)
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        SafeTreeMap.this.transformTerm(tree)(owner)
      override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
        SafeTreeMap.this.transformTypeTree(tree)(owner)
      override def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef =
        SafeTreeMap.this.transformCaseDef(tree)(owner)
      override def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef =
        SafeTreeMap.this.transformTypeCaseDef(tree)(owner)
      override def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        SafeTreeMap.this.transformStats(trees)(owner)
      override def transformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
        SafeTreeMap.this.transformTrees(trees)(owner)
      override def transformTerms(trees: List[Term])(owner: Symbol): List[Term] =
        SafeTreeMap.this.transformTerms(trees)(owner)
      override def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        SafeTreeMap.this.transformTypeTrees(trees)(owner)
      override def transformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
        SafeTreeMap.this.transformCaseDefs(trees)(owner)
      override def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        SafeTreeMap.this.transformTypeCaseDefs(trees)(owner)
      override def transformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
        SafeTreeMap.this.transformSubTrees(trees)(owner)

      def superTransformTree(tree: Tree)(owner: Symbol): Tree =
        super.transformTree(tree)(owner)
      def superTransformStatement(tree: Statement)(owner: Symbol): Statement =
        super.transformStatement(tree)(owner)
      def superTransformTerm(tree: Term)(owner: Symbol): Term =
        super.transformTerm(tree)(owner)
      def superTransformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
        super.transformTypeTree(tree)(owner)
      def superTransformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef =
        super.transformCaseDef(tree)(owner)
      def superTransformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef =
        super.transformTypeCaseDef(tree)(owner)
      def superTransformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        super.transformStats(trees)(owner)
      def superTransformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
        super.transformTrees(trees)(owner)
      def superTransformTerms(trees: List[Term])(owner: Symbol): List[Term] =
        super.transformTerms(trees)(owner)
      def superTransformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        super.transformTypeTrees(trees)(owner)
      def superTransformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
        super.transformCaseDefs(trees)(owner)
      def superTransformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        super.transformTypeCaseDefs(trees)(owner)
      def superTransformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
        super.transformSubTrees(trees)(owner)
    end underlying

    def transformTree(tree: Tree)(owner: Symbol): Tree =
      underlying.superTransformTree(tree)(owner)
    def transformStatement(tree: Statement)(owner: Symbol): Statement =
      underlying.superTransformStatement(tree)(owner)
    def transformTerm(tree: Term)(owner: Symbol): Term =
      underlying.superTransformTerm(tree)(owner)
    def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree match
      case tree: TypeBoundsTree =>
        // workaround for issue: https://github.com/lampepfl/dotty/issues/17003
        TypeBoundsTree.copy(tree)(transformTypeTree(tree.low)(owner), transformTypeTree(tree.hi)(owner)).asInstanceOf[TypeTree]
      case _ =>
        underlying.superTransformTypeTree(tree)(owner)
    def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef =
      underlying.superTransformCaseDef(tree)(owner)
    def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef =
      underlying.superTransformTypeCaseDef(tree)(owner)
    def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
      underlying.superTransformStats(trees)(owner)
    def transformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
      underlying.superTransformTrees(trees)(owner)
    def transformTerms(trees: List[Term])(owner: Symbol): List[Term] =
      underlying.superTransformTerms(trees)(owner)
    def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
      underlying.superTransformTypeTrees(trees)(owner)
    def transformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
      underlying.superTransformCaseDefs(trees)(owner)
    def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
      underlying.superTransformTypeCaseDefs(trees)(owner)
    def transformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
      underlying.superTransformSubTrees(trees)(owner)
  end SafeTreeMap

  extension (using Quotes)(printerModule: quotes.reflect.PrinterModule)
    def SafeTypeReprCode = safeTypeReprPrinter(quotes.reflect.Printer.TypeReprCode)
    def SafeTypeReprShortCode = safeTypeReprPrinter(quotes.reflect.Printer.TypeReprShortCode)

  private def safeTypeReprPrinter(using Quotes)(printer: quotes.reflect.Printer[quotes.reflect.TypeRepr]) =
    new quotes.reflect.Printer[quotes.reflect.TypeRepr]:
      import quotes.reflect.*

      def show(tpe: TypeRepr) = showType(tpe)

      private def showType(tpe: TypeRepr): String =
        try tpe.show(using printer)
        catch case NonFatal(_) =>
          try tpe match
            case AppliedType(tycon, args) =>
              val tyconName = showType(tycon)
              val argsNames = args map showType
              if tyconName.isEmpty then ""
              else if argsNames contains "" then tyconName
              else s"$tyconName[${argsNames.mkString(", ")}]"
            case tpe: NamedType if tpe.typeSymbol != defn.RootClass =>
              val owner = tpe.typeSymbol.owner
              val qualifierName = showType(tpe.qualifier)
              if qualifierName.nonEmpty then s"$qualifierName.${tpe.name}"
              else if owner != defn.RootClass then s"${owner.fullName}.${tpe.name}"
              else tpe.name
            case AnnotatedType(underlying, annotation) =>
              val underlyingName = showType(underlying)
              val annotationName =
                try annotation.show.stripPrefix("new ")
                catch case NonFatal(_) => ""
              if annotationName.nonEmpty then s"$underlyingName @$annotationName"
              else underlyingName
            case MatchType(_, scrutinee, cases) =>
              val casesNames = cases map showType
              s"${showType(scrutinee)} match { ${casesNames.mkString(" ")} }"
            case MatchCase(pattern, rhs) =>
              s"case ${showType(pattern)} => ${showType(rhs)}"
            case ThisType(ref) if ref.typeSymbol.isPackageDef || ref.typeSymbol.isModuleDef =>
              showType(ref)
            case ThisType(ref) =>
              s"${showType(ref)}.this"
            case SuperType(ThisType(ref), _) =>
              s"${showType(ref)}.super"
            case SuperType(ref, _) =>
              s"${showType(ref)}.super"
            case Refinement(parent: Refinement, name, info) =>
              s"${showType(parent).stripSuffix(" }")}; ${showRefinement(name, info)} }"
            case Refinement(parent, name, info) =>
              s"${showType(parent)} { ${showRefinement(name, info)} }"
            case ByNameType(underlying) =>
              s"=> ${showType(underlying)}"
            case tpe @ MethodType(_, _, _: MethodOrPoly) =>
              showLambdaType(tpe, "(", ":", ")")
            case tpe: MethodType =>
              showLambdaType(tpe, "(", ":", "): ")
            case tpe @ PolyType(_, _, _: MethodOrPoly) =>
              showLambdaType(tpe, "[", "", "]")
            case tpe: PolyType =>
              showLambdaType(tpe, "[", "", "]: ")
            case tpe: TypeLambda =>
              showLambdaType(tpe, "[", "", "] =>> ")
            case ParamRef(binder: LambdaType, paramNum) =>
              binder.paramNames(paramNum)
            case tpe: AndType =>
              showAndOrType(tpe, "&")
            case tpe: OrType =>
              showAndOrType(tpe, "|")
            case TypeBounds(low, hi) =>
              s">: ${showType(low)} <: ${showType(hi)}"
            case ConstantType(constant) =>
              constant.value.toString
            case _ =>
              ""
          catch case NonFatal(_) => ""

      private def showLambdaType(tpe: LambdaType, open: String, mid: String, close: String) =
        val args = tpe.paramNames zip tpe.paramTypes map { (name, tpe) => s"$name$mid ${showType(tpe)}" }
        s"${args.mkString(open, ", ", close)}${showType(tpe.resType)}"

      private def showAndOrType(tpe: AndOrType, operator: String) =
        val leftName = showType(tpe.left)
        val rightName = showType(tpe.right)
        if leftName.nonEmpty && rightName.nonEmpty then s"($leftName $operator $rightName)"
        else if leftName.nonEmpty then leftName
        else if rightName.nonEmpty then rightName
        else ""

      private def showRefinement(name: String, info: TypeRepr) = info match
        case _: TypeBounds =>
          s"type $name ${showType(info)}"
        case ByNameType(underlying) =>
          s"def $name: ${showType(underlying)}"
        case _: MethodOrPoly =>
          s"def $name${showType(info)}"
        case _ =>
          s"val $name: ${showType(info)}"
  end safeTypeReprPrinter

  private object LazyRefStripping:
    private val stripLazyRef =
      try
        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
        val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")

        val ctx = quotesImplClass.getMethod("ctx")
        val stripLazyRef = typeClass.getMethod("stripLazyRef", contextClass)

        { (quotes: Quotes, tpe: Any) =>
          import quotes.reflect.*

          try
            if typeClass.isInstance(tpe) then
              stripLazyRef.invoke(tpe, ctx.invoke(quotes)) match
                case tpe: TypeRepr @unchecked if typeClass.isInstance(tpe) =>
                  Some(tpe)
                case _ =>
                  None
            else
              None
          catch { case _: IllegalArgumentException | _: ClassCastException => None }
        }
      catch
        case _: ClassNotFoundException | _: NoSuchMethodException =>
          (quotes: Quotes, tpe: Any) => None

    def strip(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      stripLazyRef(quotes, tpe) getOrElse tpe
  end LazyRefStripping

  private object TypeParamSubstition:
    private val substituteTypeParam =
      try
        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
        val typeClass = Class.forName("dotty.tools.dotc.core.Types$Type")
        val paramRefClass = Class.forName("dotty.tools.dotc.core.Types$ParamRef")

        val ctx = quotesImplClass.getMethod("ctx")
        val substParam = typeClass.getMethod("substParam", paramRefClass, typeClass, contextClass)

        { (quotes: Quotes, tpe: Any, from: Any, to: Any) =>
          import quotes.reflect.*

          try
            if paramRefClass.isInstance(from) && typeClass.isInstance(to) && typeClass.isInstance(tpe) then
              substParam.invoke(tpe, from, to, ctx.invoke(quotes)) match
                case tpe: TypeRepr @unchecked if typeClass.isInstance(tpe) =>
                  Some(tpe)
                case _ =>
                  None
            else
              None
          catch { case _: IllegalArgumentException | _: ClassCastException => None }
        }
      catch
        case _: ClassNotFoundException | _: NoSuchMethodException =>
          (quotes: Quotes, tpe: Any, from: Any, to: Any) => None

    def substitute(using Quotes)(tpe: quotes.reflect.TypeRepr, from: quotes.reflect.ParamRef, to: quotes.reflect.TypeRepr) =
      substituteTypeParam(quotes, tpe, from, to) getOrElse tpe
  end TypeParamSubstition

  private object TypeArgVariances:
    private def varianceFlags(using Quotes)(flags: quotes.reflect.Flags) =
      import quotes.reflect.*
      if (flags is Flags.Covariant) && !(flags is Flags.Contravariant) then Flags.Covariant
      else if (flags is Flags.Contravariant) && !(flags is Flags.Covariant) then Flags.Contravariant
      else Flags.EmptyFlags

    private def variancesOfSymbol(using Quotes)(symbol: quotes.reflect.Symbol) =
      Some(symbol.declaredTypes collect { case symbol if symbol.isTypeParam => varianceFlags(symbol.flags) })

    private val variancesOfType =
      try
        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
        val typeLambdaClass = Class.forName("dotty.tools.dotc.core.Types$TypeLambda")
        val lambdaParamClass = Class.forName("dotty.tools.dotc.core.Types$LambdaParam")

        val ctx = quotesImplClass.getMethod("ctx")
        val typeParams = typeLambdaClass.getMethod("typeParams")
        val paramVariance = lambdaParamClass.getMethod("paramVariance", contextClass)

        { (quotes: Quotes, tpe: Any) =>
          import quotes.reflect.*

          try
            if typeLambdaClass.isInstance(tpe) then
              typeParams.invoke(tpe) match
                case params: List[_] =>
                  val flagsClass = Flags.EmptyFlags.getClass
                  val context = ctx.invoke(quotes)
                  val variances = params map { param =>
                    paramVariance.invoke(param, context) match
                      case flags: Flags @unchecked if flagsClass.isInstance(flags) =>
                        Some(varianceFlags(using quotes)(flags))
                      case _ =>
                        None
                  }

                  if variances contains None then None
                  else Some(variances.flatten)

                case _ =>
                  None
            else
              None
          catch { case _: IllegalArgumentException | _: ClassCastException => None }
        }
      catch
        case _: ClassNotFoundException | _: NoSuchMethodException =>
          (quotes: Quotes, tpe: Any) => None

    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr) =
      import quotes.reflect.*

      tpe match
        case tpe: LambdaType => variancesOfType(quotes, tpe)
        case TypeBounds(_, tpe: LambdaType) => variancesOfType(quotes, tpe)
        case ParamRef(PolyType(_, bounds, _), paramNum) =>
          bounds(paramNum).hi match
            case tpe: LambdaType => variancesOfType(quotes, tpe)
            case _ => variancesOfSymbol(tpe.typeSymbol)
        case _ => variancesOfSymbol(tpe.typeSymbol)
    end unapply
  end TypeArgVariances
end reflectionExtensions
