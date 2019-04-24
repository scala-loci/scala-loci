package loci.dev
package language
package impl

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Multitier {
  def compilationFailure(message: String): Unit = macro compilationFailureImpl

  def compilationFailureImpl(c: blackbox.Context)(message: c.Tree): c.Tree = {
    import c.universe._

    message match {
      case Literal(Constant(constant)) =>
        c.abort(c.enclosingPosition, constant.toString)
      case _ =>
        c.abort(c.enclosingPosition, message.toString)
    }
  }
}

class Multitier(val c: blackbox.Context) {
  import c.universe._

  def annotation(annottees: Tree*): Tree = {
    val multitierAnnotationType = c.mirror.staticClass("_root_.loci.dev.multitier").toType

    val isNestedExpansion = c.openMacros exists { other =>
      other.macroApplication match {
        case q"new $macroApplication(...$_).macroTransform(...$_)"
            if c.enclosingPosition.source.file.path == other.enclosingPosition.source.file.path &&
               (other.macroApplication exists { _.pos == c.enclosingPosition }) &&
               (c.enclosingPosition != other.enclosingPosition ||
                c.macroApplication.toString != other.macroApplication.toString) =>
          c.typecheck(macroApplication, c.TYPEmode, silent = true).tpe =:= multitierAnnotationType

        case _ =>
          false
      }
    }

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { other =>
      c.enclosingPosition == other.enclosingPosition &&
      c.macroApplication.toString == other.macroApplication.toString
    }
    val isRecursiveExpansion = recursionCount > 2

    val (annottee, companion) = annottees match {
      case (annottee: ImplDef) :: companion => (annottee, companion)
      case _ =>
        c.abort(c.enclosingPosition,
          "multitier annotation only applicable to classes, traits or objects")
    }

    val processedAnnotee: Tree =
      if (!c.hasErrors && !isRecursiveExpansion && !isNestedExpansion) {
        try {
          import preprocessors._
          import components._
          import retypecheck._

          val retyper = c.retyper

          val preprocessedAnnottee = Preprocessor.run(c)(
            annottee,
            Seq(MultitierTypes))

          val typedAnnottee =
            try retyper typecheck preprocessedAnnottee match { case tree: ImplDef => tree }
            catch improveTypecheckingErrorMessage

          fixAnnotteeSymbols(typedAnnottee)

          val Engine.Result(engine, records) = Engine.run(c)(
            typedAnnottee,
            Seq(Commons, ModuleInfo, Initialization, RemoteBlock, Peers, Values, RemoteAccess, Impls, Assembly))

          val assembly = engine.require(Assembly)

          (records
            collectFirst { case assembly.Assembly(annottee) =>
              retyper untypecheckAll annottee
            }
            getOrElse annottee)
        }
        catch improveMacroErrorReporting(annottee)
      }
      else if (!c.hasErrors && !isRecursiveExpansion) {
        val mods = annottee.mods mapAnnotations {
          q"new ${termNames.ROOTPKG}.loci.dev.runtime.MultitierModule" :: _
        }

        annottee match {
          case ModuleDef(_, name, impl) =>
            treeCopy.ModuleDef(annottee, mods, name, impl)
          case ClassDef(_, name, tparams, impl) =>
            treeCopy.ClassDef(annottee, mods, name, tparams, impl)
        }
      }
      else
        annottee

    (companion.headOption
      map { companion => q"$processedAnnotee; $companion"}
      getOrElse processedAnnotee)
  }

  private def fixAnnotteeSymbols(annottee: Tree) = {
    val fixingSymbols = mutable.Set.empty[Symbol]

    def modulePath(symbol: Symbol): Option[List[Symbol]] =
      if (annottee.symbol.info.baseClasses contains symbol.owner)
        None
      else if (symbol.owner == annottee.symbol.owner)
        Some(List(symbol))
      else if (symbol.owner.isTerm && (!symbol.owner.isMethod || symbol.owner.asMethod.paramLists.isEmpty))
        modulePath(symbol.owner) map { symbol :: _ }
      else if (symbol.owner.isModuleClass)
        modulePath(symbol.owner.asClass.module) map { symbol :: _ }
      else
        None

    def sameSignature(symbol0: Symbol, symbol1: Symbol) =
      (symbol0.isType && symbol1.isType) ||
      (symbol0.isTerm && symbol1.isTerm &&
       !symbol0.isMethod && !symbol1.isMethod) ||
      (symbol0.isMethod && symbol1.isMethod &&
       (symbol0.asMethod.paramLists map { _  map { _.fullName } }) ==
       (symbol1.asMethod.paramLists map { _  map { _.fullName } }))

    def fixSymbol(symbol: Symbol): Option[Symbol] = {
      fixSymbolInfo(symbol)

      modulePath(symbol) flatMap { list =>
        if (list.last.name.toString == annottee.symbol.name.toString) {
          val fixedSymbol = list.dropRight(1).foldRight(annottee.symbol) { (treeSymbol, symbol) =>
            ((symbol.info member treeSymbol.name).alternatives
                collectFirst { case symbol if sameSignature(symbol, treeSymbol) => symbol }
                getOrElse NoSymbol)
          }

          if (fixedSymbol != NoSymbol) {
            val fixedFlippedSymbol =
              if (symbol.isModuleClass && fixedSymbol.isModule)
                fixedSymbol.asModule.moduleClass
              else if (symbol.isModule && fixedSymbol.isModuleClass)
                fixedSymbol.asClass.module
              else
                fixedSymbol

            if (fixedFlippedSymbol != symbol) {
              fixSymbolInfo(fixedFlippedSymbol)
              Some(fixedFlippedSymbol)
            }
            else
              None
          }
          else
            None
        }
        else
          None
      }
    }

    def fixSymbolInfo(symbol: Symbol): Unit =
      if (!(fixingSymbols contains symbol)) {
        fixingSymbols += symbol
        fixType(symbol.info) foreach { internal.setInfo(symbol, _) }
        symbol.overrides foreach fixSymbolInfo
      }

    def fixType(tpe: Type): Option[Type] = tpe match {
      case AnnotatedType(annotations, underlying) =>
        annotations foreach { annotation => traverser traverse annotation.tree }
        fixType(underlying) map { internal.annotatedType(annotations, _) }

      case BoundedWildcardType(TypeBounds(lo, hi)) =>
        val opLo = fixType(lo)
        val opHi = fixType(hi)
        if (opLo.nonEmpty || opHi.nonEmpty)
          Some(internal.boundedWildcardType(internal.typeBounds(opLo getOrElse lo, opHi getOrElse hi)))
        else
          None

      case ClassInfoType(parents, decls, typeSymbol) =>
        val opParents = parents map { parent => fixType(parent) -> parent }
        val opTypeSymbol = fixSymbol(typeSymbol)
        if ((opParents exists { case (opParent, _) => opParent.nonEmpty }) || opTypeSymbol.nonEmpty)
          Some(internal.classInfoType(
            opParents map { case (opParent, parent) => opParent getOrElse parent },
            decls,
            opTypeSymbol getOrElse typeSymbol))
        else
          None

      case ExistentialType(quantified, underlying) =>
        val opQuantified = quantified map { sym => fixSymbol(sym) -> sym }
        val opUnderlying = fixType(underlying)
        if ((opQuantified exists { case (opSym, sym) => opSym.nonEmpty }) || opQuantified.nonEmpty)
          Some(internal.existentialType(
            opQuantified map { case (opSym, sym) => opSym getOrElse sym },
            opUnderlying getOrElse underlying))
        else
          None

      case MethodType(params, resultType) =>
        val opParams = params map { param => fixSymbol(param) -> param }
        val opResultType = fixType(resultType)
        if ((opParams exists { case (opParam, _) => opParam.nonEmpty }) || opResultType.nonEmpty)
          Some(internal.methodType(
            opParams map { case (opParam, param) => opParam getOrElse param },
            opResultType getOrElse resultType))
        else
          None

      case NullaryMethodType(resultType) =>
        fixType(resultType) map internal.nullaryMethodType

      case PolyType(typeParams, resultType) =>
        val opTypeParams = typeParams map { typeParam => fixSymbol(typeParam) -> typeParam }
        val opResultType = fixType(resultType)
        if ((opTypeParams exists { case (opTypeParam, _) => opTypeParam.nonEmpty }) || opResultType.nonEmpty)
          Some(internal.polyType(
            opTypeParams map { case (opTypeParam, typeParam) => opTypeParam getOrElse typeParam },
            opResultType getOrElse resultType))
        else
          None

      case RefinedType(parents, scope) =>
        val opParents = parents map { parent => fixType(parent) -> parent }
        if (opParents exists { case (opParent, _) => opParent.nonEmpty })
          Some(internal.refinedType(
            opParents map { case (opParent, parent) => opParent getOrElse parent },
            scope))
        else
          None

      case SingleType(pre, sym) =>
        val opPre = fixType(pre)
        val opSym = fixSymbol(sym)
        if (opPre.nonEmpty || opSym.nonEmpty)
          Some(internal.singleType(opPre getOrElse pre, opSym getOrElse sym))
        else
          None

      case SuperType(thistpe: Type, supertpe: Type) =>
        val opThistpe = fixType(thistpe)
        val opSupertpe = fixType(supertpe)
        if (opThistpe.nonEmpty || opSupertpe.nonEmpty)
          Some(internal.superType(opThistpe getOrElse thistpe, opSupertpe getOrElse supertpe))
        else
          None

      case ThisType(pre) =>
        fixSymbol(pre) map internal.thisType

      case TypeBounds(lo, hi) =>
        val opLo = fixType(lo)
        val opHi = fixType(hi)
        if (opLo.nonEmpty || opHi.nonEmpty)
          Some(internal.typeBounds(opLo getOrElse lo, opHi getOrElse hi))
        else
          None

      case TypeRef(pre, sym, args) =>
        val opPre = fixType(pre)
        val opSym = fixSymbol(sym)
        val opArgs = args map { arg => fixType(arg) -> arg }
        if (opPre.nonEmpty || opSym.nonEmpty || (opArgs exists { case (opArg, _) => opArg.nonEmpty }))
          Some(internal.typeRef(
            opPre getOrElse pre,
            opSym getOrElse sym,
            opArgs map { case (opArg, arg) => opArg getOrElse arg }))
        else
          None

      case _ =>
        None
    }

    object traverser extends Traverser {
      override def traverse(tree: Tree) = {
        tree match {
          case tree: TypeTree =>
            if (tree.original != null)
              traverse(tree.original)
          case _ =>
            if (tree.symbol != null)
              fixSymbol(tree.symbol) foreach { internal.setSymbol(tree, _) }
        }

        if (tree.tpe != null)
          fixType(tree.tpe) foreach { internal.setType(tree, _) }

        super.traverse(tree)
      }
    }

    if (annottee.symbol.isModule)
      traverser traverse annottee
  }

  private def readPosField(e: Throwable) =
    try e.getClass.getMethod("pos").invoke(e) match {
      case pos: Position @unchecked => pos
      case _ => throw e
    }
    catch { case _: NoSuchFieldException => throw e }

  private def improveMacroErrorReporting(annottee: Tree): PartialFunction[Throwable, Tree] = {
    case e: Throwable
        if e.getClass.getCanonicalName == "scala.reflect.macros.runtime.AbortMacroException" =>

      val pos = readPosField(e)

      val compilationError =
        q"${termNames.ROOTPKG}.loci.dev.language.impl.Multitier.compilationFailure(${e.getMessage})"

      annottee match {
        case ModuleDef(mods, name, _) =>
          atPos(pos) { q"$mods object $name { $compilationError }" }
        case ClassDef(mods, name, _, _) if mods hasFlag Flag.TRAIT =>
          atPos(pos) { q"$mods trait $name { $compilationError }" }
        case ClassDef(mods, name, _, _) =>
          atPos(pos) { q"$mods class $name { $compilationError }" }
      }
  }

  private val improveTypecheckingErrorMessage: PartialFunction[Throwable, Nothing] = {
    case e: Throwable
        if e.getClass.getCanonicalName == "scala.reflect.macros.runtime.AbortMacroException" &&
           (e.getMessage contains "type mismatch") =>

      val pos = readPosField(e)

      def clean(line: String) =
        line.replaceFirst("^.*:\\s*(.*?)\\s*$", "$1")

      def cleanPaths(lines: Array[String]) = lines flatMap { string =>
        val elements = string split "\\(in "
        if (elements.size > 1) Some(elements dropRight (elements.size / 2) mkString "(in ")
        else None
      }

      def cleanPath(lines: Array[String]) =
        if (lines.length == 2) {
          val count0 = (lines(0) split "\\(some other\\)").length
          val count1 = (lines(1) split "\\(some other\\)").length

          if (count1 == count0 + 1) Some(lines(0))
          else if (count0 == count1 + 1) Some(lines(1))
          else if (lines(1).replaceFirst("\\s*\\{\\s*\\}\\s*$", "") == lines(0)) Some(lines(0))
          else if (lines(0).replaceFirst("\\s*\\{\\s*\\}\\s*$", "") == lines(1)) Some(lines(1))
          else None
        }
        else
          None

      val lines = e.getMessage split "[\r\n]+" filter { _ contains ":" } map clean

      val cleanedPath = cleanPath(lines)

      val path = (cleanedPath
        map { path => cleanPaths(Array(path)).headOption getOrElse path }
        orElse cleanPaths(lines).headOption)

      path foreach { path =>
        c.abort(pos, s"Cycle during macro expansion involving $path")
      }

      throw e
  }
}
