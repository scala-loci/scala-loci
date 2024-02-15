package loci
package embedding

import impl.SymbolMutator

import java.lang.reflect.Field
import java.util.IdentityHashMap
import scala.quoted.*
import scala.util.control.NonFatal

final class MultitierPreprocessor

object MultitierPreprocessor:
  transparent inline given MultitierPreprocessor = ${ preprocess }

  def preprocess(using Quotes): Expr[MultitierPreprocessor] =
    import quotes.reflect.*

    val multitier = Symbol.requiredClass("loci.language.multitier")
    val deferred = Symbol.requiredClass("loci.language.deferred")
    val placed = Symbol.requiredMethod("loci.language.placed.apply")
    val compileTimeOnly = Symbol.requiredClass("scala.annotation.compileTimeOnly")
    val uninitialized = Symbol.requiredMethod("scala.compiletime.uninitialized")

    def scopeSymbol(symbol: Symbol): Boolean =
      symbol.isValDef || symbol.isDefDef || symbol.isClassDef

    def scopeOwner(symbol: Symbol): Symbol =
      if symbol.exists && (symbol.isLocalDummy || !scopeSymbol(symbol)) then
        scopeOwner(symbol.owner)
      else
        symbol

    val owner =
      if Symbol.spliceOwner.flags is Flags.Macro then
        scopeOwner(Symbol.spliceOwner.owner)
      else
        scopeOwner(Symbol.spliceOwner)

    if owner.exists then
      try
        val flagsClass = Flags.EmptyFlags.getClass
        val quotesImplClass = Class.forName("scala.quoted.runtime.impl.QuotesImpl")
        val contextClass = Class.forName("dotty.tools.dotc.core.Contexts$Context")
        val symbolClass = Class.forName("dotty.tools.dotc.core.Symbols$Symbol")
        val symDenotationClass = Class.forName("dotty.tools.dotc.core.SymDenotations$SymDenotation")
        val annotationClass = Class.forName("dotty.tools.dotc.core.Annotations$Annotation")
        val completerClass = Class.forName("dotty.tools.dotc.typer.Namer$Completer")
        val sourceFileClass = Class.forName("dotty.tools.dotc.util.SourceFile")
        val sourcePositionClass = Class.forName("dotty.tools.dotc.util.SourcePosition")
        val positionedClass = Class.forName("dotty.tools.dotc.ast.Positioned")
        val treeClass = Class.forName("dotty.tools.dotc.ast.Trees$Tree")
        val defTreeClass = Class.forName("dotty.tools.dotc.ast.Trees$DefTree")
        val blockClass = Class.forName("dotty.tools.dotc.ast.Trees$Block")
        val applyClass = Class.forName("dotty.tools.dotc.ast.Trees$Apply")
        val templateClass = Class.forName("dotty.tools.dotc.ast.Trees$Template")
        val valOrDefDefClass = Class.forName("dotty.tools.dotc.ast.Trees$ValOrDefDef")
        val valDefClass = Class.forName("dotty.tools.dotc.ast.Trees$ValDef")
        val defDefClass = Class.forName("dotty.tools.dotc.ast.Trees$DefDef")
        val typeDefClass = Class.forName("dotty.tools.dotc.ast.Trees$TypeDef")
        val appliedTypeTreeClass = Class.forName("dotty.tools.dotc.ast.Trees$AppliedTypeTree")
        val moduleDefClass = Class.forName("dotty.tools.dotc.ast.untpd$ModuleDef")
        val infixOpClass = Class.forName("dotty.tools.dotc.ast.untpd$InfixOp")
        val typedSpliceClass = Class.forName("dotty.tools.dotc.ast.untpd$TypedSplice")
        val modifiersClass = Class.forName("dotty.tools.dotc.ast.untpd$Modifiers")

        val ctx = quotesImplClass.getMethod("ctx")
        val denot = symbolClass.getMethod("denot", contextClass)
        val infoOrCompleter = symDenotationClass.getMethod("infoOrCompleter")
        val annotations = symDenotationClass.getMethod("annotations", contextClass)
        val flagsUnsafe = symDenotationClass.getMethod("flagsUNSAFE")
        val resetFlag = symDenotationClass.getMethod("resetFlag", classOf[Long])
        val annotationSymbol = annotationClass.getMethod("symbol", contextClass)
        val original = completerClass.getMethod("original")
        val contains = sourcePositionClass.getMethod("contains", sourcePositionClass)
        val sourcePos = positionedClass.getMethod("sourcePos", contextClass)
        val isEmpty = treeClass.getMethod("isEmpty")
        val rawMods = defTreeClass.getMethod("rawMods")
        val setMods = defTreeClass.getMethod("setMods", modifiersClass)
        val stats = blockClass.getMethod("stats")
        val apply = applyClass.getMethod("apply", treeClass, classOf[List[?]], sourceFileClass)
        val unforcedBody = templateClass.getMethod("unforcedBody")
        val unforcedRhs = valOrDefDefClass.getMethod("unforcedRhs")
        val tpt = valOrDefDefClass.getMethod("tpt")
        val valRhs = valDefClass.getDeclaredField("preRhs")
        val defRhs = defDefClass.getDeclaredField("preRhs")
        val rhs = typeDefClass.getMethod("rhs")
        val impl = moduleDefClass.getMethod("impl")
        val op = infixOpClass.getMethod("op")
        val typedSplice = typedSpliceClass.getMethod("apply", treeClass, classOf[Boolean], contextClass)
        val splice = typedSpliceClass.getMethod("splice")
        val modFlags = modifiersClass.getMethod("flags")
        val modAnnotations = modifiersClass.getMethod("annotations")
        val modWithAddedAnnotation = modifiersClass.getMethod("withAddedAnnotation", treeClass)

        val context = ctx.invoke(quotes)

        object Symbol:
          def unapply(symbol: Any): Option[Symbol] = symbol match
            case symbol: Symbol @unchecked if symbolClass.isInstance(symbol) => Some(symbol)
            case _ => None

        object Tree:
          def unapply(tree: Any): Option[Tree] = tree match
            case tree: Tree @unchecked if treeClass.isInstance(tree) => Some(tree)
            case _ => None

        def completerOriginalTree(symbol: Symbol) =
          val info = infoOrCompleter.invoke(denot.invoke(symbol, context))
          Option.when(completerClass.isInstance(info)) { original.invoke(info) }

        def declarationsOfSymbol(symbol: Symbol) =
          completerOriginalTree(symbol).fold(symbol.declarations)(declarationsOfTree)

        def declarationsOfTree(tree: Any) =
          val blockOrTemplate =
            if valOrDefDefClass.isInstance(tree) then Some(unforcedRhs.invoke(tree))
            else if typeDefClass.isInstance(tree) then Some(rhs.invoke(tree))
            else if moduleDefClass.isInstance(tree) then Some(impl.invoke(tree))
            else None
          val statements = blockOrTemplate map: blockOrTemplate =>
            if blockClass.isInstance(blockOrTemplate) then stats.invoke(blockOrTemplate)
            else if templateClass.isInstance(blockOrTemplate) then unforcedBody.invoke(blockOrTemplate)
            else List.empty
          statements match
            case Some(stats: List[?]) =>
              stats filter: stat =>
                valDefClass.isInstance(stat) ||
                defDefClass.isInstance(stat) ||
                typeDefClass.isInstance(stat) ||
                moduleDefClass.isInstance(stat)
            case _ =>
              List.empty

        def declarations(decl: Any) = decl match
          case Symbol(symbol) => declarationsOfSymbol(symbol)
          case _ => declarationsOfTree(decl)

        def hasAnnotation(decl: Any)(predicate: Any => Boolean) = decl match
          case Symbol(symbol) => annotations.invoke(denot.invoke(symbol, context), context) match
            case annotations: List[?] => annotations exists { annotation => predicate(annotationSymbol.invoke(annotation, context)) }
            case _ => false
          case tree => modAnnotations.invoke(rawMods.invoke(tree)) match
            case trees: List[?] => trees exists predicate
            case _=> false

        def mayHaveAnnotationSymbol(decl: Any, annotationSymbol: Symbol) =
          val name = annotationSymbol.name
          hasAnnotation(decl):
            case Symbol(symbol) => symbol == annotationSymbol
            case tree if typedSpliceClass.isInstance(tree) => splice.invoke(tree) match
              case Tree(Apply(Select(New(tpt), _), _)) => tpt.symbol == annotationSymbol
              case _ => false
            case Tree(Apply(Select(New(TypeIdent(`name`) | TypeSelect(_, `name`)), _), _)) => true
            case _ => false

        def isMultitierAnnottee(decl: Any) = hasAnnotation(decl):
          case Symbol(symbol) => symbol == multitier
          case tree => contains.invoke(sourcePos.invoke(tree, context), Position.ofMacroExpansion) == true

        def flags(decl: Any) =
          val flags = decl match
            case Symbol(symbol) => flagsUnsafe.invoke(denot.invoke(symbol, context))
            case tree => modFlags.invoke(rawMods.invoke(tree))
          flags match
            case flags: Flags @unchecked if flagsClass.isInstance(flags) => flags
            case _ => Flags.EmptyFlags

        def mutateValOrDefRhs(valOrDefRhs: Field, tree: Any, rhs: Any) =
          try
            valOrDefRhs.setAccessible(true)
            valOrDefRhs.set(tree, rhs)
          catch
            case NonFatal(e) =>

        def maybePlacementTypeConstructorTree(tree: Any) = tree match
          case Tree(
              TypeIdent("on") |
              TypeSelect(Ident("language"), "on") |
              TypeSelect(Select(Ident("loci"), "language"), "on") |
              TypeSelect(Select(Select(Ident("_root_"), "loci"), "language"), "on") |
              TypeSelect(Ident("embedding"), "on") |
              TypeSelect(Select(Ident("loci"), "embedding"), "on") |
              TypeSelect(Select(Select(Ident("_root_"), "loci"), "embedding"), "on")) =>
            true
          case _ =>
            false

        def maybePlacementTypeTree(tree: Any) = tree match
          case _ if infixOpClass.isInstance(tree) => maybePlacementTypeConstructorTree(op.invoke(tree))
          case Tree(Applied(tpt, List(_, _))) => maybePlacementTypeConstructorTree(tpt)
          case _ => false

        def TypedSplice(tree: Tree) =
          typedSplice.invoke(null, tree, false, context)

        val placedValueCompileTimeOnlyAnnotation =
          val message = "Access to abstraction only allowed on peers on which the abstraction is placed. Remote access must be explicit."
          New(TypeIdent(compileTimeOnly)).select(compileTimeOnly.primaryConstructor).appliedTo(Literal(StringConstant(message)))

        val objectMemberCompileTimeOnlyAnnotation =
          val message = "Access to object member of multitier module not allowed."
          New(TypeIdent(compileTimeOnly)).select(compileTimeOnly.primaryConstructor).appliedTo(Literal(StringConstant(message)))

        val deferredAnnotation =
          New(TypeIdent(deferred)).select(compileTimeOnly.primaryConstructor).appliedToNone

        val processedDeclarations = IdentityHashMap[Any, Any]

        def processSymbol(decl: Any, multitierAnnottee: Boolean, compileTimeOnlyAnnotation: Term, symbol: Symbol): Unit =
          if (symbol.isValDef || symbol.isDefDef) &&
             !(flags(symbol) is Flags.Module) &&
             !symbol.isClassConstructor then
            completerOriginalTree(symbol) foreach:
              processTree(decl, multitierAnnottee, compileTimeOnlyAnnotation, _)

            if multitierAnnottee && (flags(decl) is Flags.Module) && (flags(symbol) is Flags.Deferred) then
              resetFlag.invoke(denot.invoke(symbol, context), Flags.Deferred)
              if !mayHaveAnnotationSymbol(symbol, deferred) then
                SymbolMutator.get.get.updateAnnotationWithTree(symbol, deferredAnnotation)

            if !mayHaveAnnotationSymbol(symbol, compileTimeOnly) then
              SymbolMutator.get.get.updateAnnotationWithTree(symbol, compileTimeOnlyAnnotation)

          else if symbol.isClassDef && (isMultitierAnnottee(symbol) || (flags(symbol) is Flags.Module)) then
            processDeclarations(symbol)
        end processSymbol

        def processTree(decl: Any, multitierAnnottee: Boolean, compileTimeOnlyAnnotation: Term, tree: Any): Unit =
          if valOrDefDefClass.isInstance(tree) then
            val rhs = unforcedRhs.invoke(tree)
            if isEmpty.invoke(rhs) == true then
              if multitierAnnottee && (flags(decl) is Flags.Module) then
                if !mayHaveAnnotationSymbol(tree, deferred) then
                  setMods.invoke(tree, modWithAddedAnnotation.invoke(rawMods.invoke(tree), TypedSplice(deferredAnnotation)))
                if valDefClass.isInstance(tree) then
                  mutateValOrDefRhs(valRhs, tree, TypedSplice(Ref(uninitialized)))
                if defDefClass.isInstance(tree) then
                  mutateValOrDefRhs(defRhs, tree, TypedSplice(Ref(uninitialized)))
            else if maybePlacementTypeTree(tpt.invoke(tree)) then
              def placedRhs = apply.invoke(null, TypedSplice(Ref(placed)), List(rhs), Position.ofMacroExpansion.sourceFile)
              if valDefClass.isInstance(tree) then
                mutateValOrDefRhs(valRhs, tree, placedRhs)
              if defDefClass.isInstance(tree) then
                mutateValOrDefRhs(defRhs, tree, placedRhs)

            if !mayHaveAnnotationSymbol(tree, compileTimeOnly) then
              setMods.invoke(tree, modWithAddedAnnotation.invoke(rawMods.invoke(tree), TypedSplice(compileTimeOnlyAnnotation)))

          else if moduleDefClass.isInstance(tree) ||
                  (typeDefClass.isInstance(tree) &&
                   templateClass.isInstance(rhs.invoke(tree)) &&
                   isMultitierAnnottee(tree)) then
            processDeclarations(tree)
        end processTree

        def processDeclarations(decl: Any): Unit =
          if !(processedDeclarations containsKey decl) then
            processedDeclarations.put(decl, decl)

            val multitierAnnottee = isMultitierAnnottee(decl)
            val compileTimeOnlyAnnotation =
              if mayHaveAnnotationSymbol(decl, multitier) then
                placedValueCompileTimeOnlyAnnotation
              else
                objectMemberCompileTimeOnlyAnnotation

            declarations(decl) foreach:
              case Symbol(symbol) => processSymbol(decl, multitierAnnottee, compileTimeOnlyAnnotation, symbol)
              case tree => processTree(decl, multitierAnnottee, compileTimeOnlyAnnotation, tree)
        end processDeclarations

        declarations(owner) foreach: decl =>
          if isMultitierAnnottee(decl) then
            processDeclarations(decl)

      catch
        case NonFatal(e) =>

    '{ MultitierPreprocessor() }
  end preprocess
end MultitierPreprocessor