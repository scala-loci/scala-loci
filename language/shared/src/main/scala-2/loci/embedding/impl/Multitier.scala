package loci
package embedding
package impl

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.util.Properties
import scala.util.control.NonFatal

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
  import retypecheck._

  val logging = Logging(c)
  val retyper = c.retyper

  def annotation(annottees: Tree*): Tree = {
    val multitierAnnotationType = c.mirror.staticClass("_root_.loci.language.multitier").toType
    val macroApplicationString = c.macroApplication.toString

    val isNestedExpansion = c.openMacros exists { other =>
      other.macroApplication match {
        case q"new $macroApplication(...$_).macroTransform(...$_)"
            if other.enclosingPosition.source.file.path == c.enclosingPosition.source.file.path &&
               (other.enclosingPosition != c.enclosingPosition ||
                other.macroApplication.toString != macroApplicationString) =>
          val positions = other.macroApplication collect { case tree if tree.pos != NoPosition => tree.pos }
          val min = positions minBy { pos => math.min(pos.point, pos.start) }
          val max = positions maxBy { pos => math.max(pos.point, pos.end) }

          c.enclosingPosition.point >= math.min(min.point, min.start) &&
          c.enclosingPosition.point <= math.max(max.point, max.end) &&
          c.typecheck(macroApplication, c.TYPEmode, silent = true).tpe =:= multitierAnnotationType

        case _ =>
          false
      }
    }

    val instance = symbolOf[language.Instance[_]].companion
    val sourceContent = c.enclosingPosition.source.content

    def hasInstanceOwner(symbol: Symbol): Boolean = {
      symbol.isClass && symbol.pos != NoPosition && {
        val selection = sourceContent.slice(
          pathEndpoint(sourceContent, symbol.pos.point, -1),
          pathEndpoint(sourceContent, symbol.pos.point, 1)).mkString

        try { (c typecheck (c parse selection)).symbol == instance }
        catch {
          case _: reflect.macros.ParseException |
               _: reflect.macros.TypecheckException |
               _: reflect.internal.Symbols#CyclicReference =>
            false
        }
      } || symbol != NoSymbol && hasInstanceOwner(symbol.owner)
    }

    val documentationCompiler = c.compilerSettings sliding 2 exists {
      case Seq(flag, value) =>
        flag == "-d" && ((value endsWith "/api") || (value endsWith "\\api"))
      case _ =>
        false
    }

    // the current macro expansion always appears twice
    // see: http://stackoverflow.com/a/20466423
    val recursionCount = c.openMacros.count { other =>
      c.enclosingPosition == other.enclosingPosition &&
      c.macroApplication.toString == other.macroApplication.toString
    }
    val isRecursiveExpansion = recursionCount > 2

    val constructorParams = mutable.ListBuffer.empty[List[List[ValDef]]]

    val (annottee, companion) = annottees match {
      case ClassDef(mods, tpname, tparams, impl @ Template(parents, self, _)) :: companion =>
        val body = impl.body map {
          case tree @ DefDef(mods, termNames.CONSTRUCTOR, tparams, _, tpt, rhs) =>
            constructorParams += tree.vparamss

            val vparamss = tree.vparamss map {
              _ map { tree =>
                val ValDef(mods, name, tpt, rhs) = tree: @unchecked
                if ((mods hasFlag Flag.DEFAULTPARAM) && rhs.nonEmpty)
                  treeCopy.ValDef(tree,
                    retyper.cleanModifiers(mods, removeFlags = Flag.DEFAULTINIT | Flag.DEFAULTPARAM),
                    name, tpt, EmptyTree)
                else
                  tree
              }
            }

            treeCopy.DefDef(tree, mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs)

          case tree =>
            tree
        }

        treeCopy.ClassDef(annottees.head, mods, tpname, tparams,
          treeCopy.Template(impl, parents, self, body)) -> companion

      case (annottee: ModuleDef) :: companion =>
        annottee -> companion

      case _ =>
        c.abort(c.enclosingPosition,
          "multitier annotation only applicable to classes, traits or objects")
    }

    val skip = annottee.mods.annotations exists {
      case q"new $expr[..$_](...$_)" =>
        expr equalsStructure tq"${termNames.ROOTPKG}.loci.runtime.AbstractValue"
      case _ =>
        false
    }

    val expandMultitierMacro =
      !c.hasErrors &&
      !isRecursiveExpansion &&
      !isNestedExpansion &&
      !hasInstanceOwner(c.internal.enclosingOwner) &&
      !skip

    val markMultitierMacro =
      !expandMultitierMacro &&
      !c.hasErrors &&
      !isRecursiveExpansion

    val processedAnnotee: Tree = try {
      import preprocessors._
      import components._

      val preprocessedAnnottee = Preprocessor.run(c)(
        annottee,
        Seq(MultitierTypes, AbstractValues, ImplicitContext, SelectionTupling, StatedTyping))

      if (expandMultitierMacro) {
        try {
          logging.info(
            s"Expanding multitier code for ${c.internal.enclosingOwner.fullName}.${annottee.name} " +
            s"in ${c.enclosingPosition.source.file.path}")

          if (!documentationCompiler) {
            val typedAnnottee =
              try {
                val tree =
                  argumentIntegrator transform
                   (retyper typecheck
                     (argumentExtractor transform preprocessedAnnottee))

                integrateAnnotteeArgument(tree)

                (tree: @unchecked) match { case tree: ImplDef => tree }
              }
              catch improveTypecheckingErrorMessage

            retyper.fixTypedAnnotteeSymbols(typedAnnottee)

            val Engine.Result(engine, records) = Engine.run(c)(
              typedAnnottee,
              Seq(
                Commons,
                ModuleInfo,
                Initialization,
                Peers,
                Values,
                RemoteBlock,
                RemoteAccess,
                GatewayAccess,
                RuntimeAccess,
                Subjectivity,
                Impls,
                Assembly))

            val assembly = engine.require(Assembly)

            (records
              collectFirst { case assembly.Assembly(annottee) =>
                val tree = retyper untypecheckAll annottee
                logging.debug(s"Multitier code expanded for ${c.internal.enclosingOwner.fullName}.${annottee.name}")
                tree
              }
              getOrElse annottee)
          }
          else
            preprocessedAnnottee
        }
        catch improveMacroErrorReporting(preprocessedAnnottee)
      }
      else if (markMultitierMacro) {
        val mods = annottee.mods mapAnnotations {
          q"new ${termNames.ROOTPKG}.loci.runtime.MultitierModule" :: _
        }

        (annottee: @unchecked) match {
          case ModuleDef(_, name, impl) =>
            treeCopy.ModuleDef(annottee, mods, name, impl)
          case ClassDef(_, name, tparams, impl) =>
            treeCopy.ClassDef(annottee, mods, name, tparams, impl)
        }
      }
      else
        preprocessedAnnottee
    }
    catch improveMacroErrorReporting(annottee)

    val recoveredAnnottee = processedAnnotee match {
      case ClassDef(mods, tpname, tparams, impl @ Template(parents, self, _)) if expandMultitierMacro =>
        val body = impl.body map {
          case tree @ DefDef(mods, termNames.CONSTRUCTOR, tparams, _, tpt, rhs)
              if constructorParams.nonEmpty =>
            val vparamss = constructorParams.remove(0)
            treeCopy.DefDef(tree, mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs)

          case tree =>
            tree
        }

        treeCopy.ClassDef(annottees.head, mods, tpname, tparams,
          treeCopy.Template(impl, parents, self, body))

      case _ =>
        processedAnnotee
    }

    if (expandMultitierMacro && logging.codeEnabled) {
      val name = s"${c.internal.enclosingOwner.fullName}.${annottee.name}"
      val code = (recoveredAnnottee.toString.linesWithSeparators map { "  " + _ }).mkString
      logging.code(s"Expanded code for multitier module $name:${Properties.lineSeparator}$code")
    }

    companion.headOption.fold(recoveredAnnottee) { companion => q"$recoveredAnnottee; $companion" }
  }

  private def singleOptionalArgument(exprss: List[List[Tree]], pos: Position): Option[Tree] =
    if (exprss.size == 1 && exprss.head.size == 1)
      Some(exprss.head.head)
    else if (exprss.size > 1 || exprss.size == 1 && exprss.head.size > 1)
      c.abort(pos, "wrong number of arguments")
    else
      None

  private def accessorGeneration(symbol: Symbol): Option[AccessorGeneration] = {
    def checkSymbol[T <: AccessorGeneration with Singleton: WeakTypeTag](value: T) =
      if (symbol == symbolOf[T].asClass.module) Some(value) else None

    checkSymbol(AccessorGeneration.Deferred) orElse
    checkSymbol(AccessorGeneration.Preferred) orElse
    checkSymbol(AccessorGeneration.Required) orElse
    checkSymbol(AccessorGeneration.Forced)
  }

  private def integrateAnnotteeArgument(annottee: Tree): Unit = {
    val q"new $expr(...$exprss).macroTransform(...$_)" = c.macroApplication: @unchecked

    singleOptionalArgument(exprss, expr.pos) foreach { arg =>
      val q"..$_; $_($expr)" =
        c typecheck atPos(arg.pos) {
          q"""def $$loci$$argument(accessorGeneration: ${typeOf[AccessorGeneration]}): Unit =
            ${termNames.ROOTPKG}.scala.Predef.locally(accessorGeneration)
          $$loci$$argument($arg)"""
        }: @unchecked

      accessorGeneration(expr.symbol) foreach { internal.updateAttachment(annottee, _) }
    }
  }

  private object argumentExtractor extends Transformer {
    val accessorGeneration = typeOf[AccessorGeneration]
    var index = 0

    override def transform(tree: Tree): Tree = tree match {
      case tree @ Template(parents, self, _) =>
        val body = tree.body flatMap {
          case tree: ImplDef =>
            val exprss = tree.mods.annotations collectFirst {
              case tree @ q"new multitier(...$exprss)" => exprss -> tree.pos
              case tree @ q"new $_.multitier(...$exprss)" => exprss -> tree.pos
            }

            (exprss
              flatMap (singleOptionalArgument _).tupled
              map { arg =>
                val name = TermName(s"$$loci$$argument$$$index")
                index += 1
                Seq(
                  atPos(arg.pos) {
                    q"""${Flag.SYNTHETIC} private[this] def $name(accessorGeneration: $accessorGeneration): Unit =
                        ${termNames.ROOTPKG}.scala.Predef.locally(accessorGeneration)""" },
                  atPos(arg.pos) {
                    q"""$name($arg)""" },
                  tree)
              }
              getOrElse Seq(tree))

          case tree =>
            Seq(tree)
        }

        super.transform(treeCopy.Template(tree, parents, self, body))

      case _ =>
        super.transform(tree)
    }
  }

  private object argumentIntegrator extends Transformer {
    val multitierModule = typeOf[runtime.MultitierModule]

    def isExtractedArgument(symbol: Symbol) =
      symbol != null && symbol.isMethod && symbol.isSynthetic &&
      (symbol.name.toString startsWith "$loci$argument$")

    override def transform(tree: Tree): Tree = tree match {
      case tree @ Template(parents, self, _) =>
        tree.body sliding 2 foreach {
          case Seq(q"$expr($arg)", module: ImplDef)
            if isExtractedArgument(expr.symbol) &&
               (module.symbol.annotations exists { _.tree.tpe <:< multitierModule }) =>
              accessorGeneration(arg.symbol) foreach { internal.updateAttachment(module, _) }

          case _ =>
        }

        val body = tree.body filterNot { tree => isExtractedArgument(tree.symbol) }

        super.transform(treeCopy.Template(tree, parents, self, body))

      case _: ImplDef =>
        val symbol =
          if (tree.symbol.isModule)
            tree.symbol.asModule.moduleClass
          else
            tree.symbol

        symbol.info match {
          case info @ ClassInfoType(parents, _, typeSymbol) =>
            val decls = internal.newScopeWith(info.decls.toSeq filterNot isExtractedArgument: _*)
            internal.setInfo(symbol, internal.classInfoType(parents, decls, typeSymbol))
          case _ =>
        }

        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
  }

  private def readPosField(e: Throwable) =
    try e.getClass.getMethod("pos").invoke(e) match {
      case pos: Position @unchecked => pos
      case _ => throw e
    }
    catch { case NonFatal(_) => throw e }

  private def improveMacroErrorReporting(annottee: Tree): PartialFunction[Throwable, Tree] = {
    case e: Throwable
      if e.getClass.getCanonicalName == "scala.reflect.macros.runtime.AbortMacroException" ||
         e.getClass.getCanonicalName == "scala.reflect.macros.TypecheckException" =>

      logging.debug(s" Expansion failed: ${e.getMessage}")

      val pos = readPosField(e)

      val compilationError = atPos(pos) {
        q"${termNames.ROOTPKG}.loci.embedding.impl.Multitier.compilationFailure(${e.getMessage})"
      }

      val compilationErrorAnnottee =
        (annottee: @unchecked) match {
          case ModuleDef(mods, name, impl @ Template(parents, self, body)) =>
            treeCopy.ModuleDef(annottee, mods, name,
              treeCopy.Template(impl, parents, self, body :+ compilationError))
          case ClassDef(mods, name, tparams, impl @ Template(parents, self, body)) =>
            treeCopy.ClassDef(annottee, mods, name, tparams,
              treeCopy.Template(impl, parents, self, body :+ compilationError))
        }

      object skipNestedExpansionMarker extends Transformer {
        val abstractValue = atPos(pos) { q"new ${termNames.ROOTPKG}.loci.runtime.AbstractValue" }

        override def transform(tree: Tree): Tree = tree match {
          case ModuleDef(mods, name, impl) =>
            super.transform(treeCopy.ModuleDef(tree, mods mapAnnotations { _ :+ abstractValue }, name, impl))
          case ClassDef(mods, name, tparams, impl) =>
            super.transform(treeCopy.ClassDef(tree, mods mapAnnotations { _ :+ abstractValue }, name, tparams, impl))
          case _ =>
            super.transform(tree)
        }
      }

      skipNestedExpansionMarker transform compilationErrorAnnottee
  }

  private val improveTypecheckingErrorMessage: PartialFunction[Throwable, Nothing] = {
    case e: Throwable
      if (e.getClass.getCanonicalName == "scala.reflect.macros.runtime.AbortMacroException" ||
          e.getClass.getCanonicalName == "scala.reflect.macros.TypecheckException") &&
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

  private def pathEndpoint(content: Array[Char], pos: Int, step: Int) = {
    var point = pos
    var current = point
    var separator = false
    var escaped = false

    while (current > 0 && current < content.length - 1)
      if (content(current) == '`') {
        if (escaped) {
          current += step
          point = current
          separator = false
          escaped = false
        }
        else {
          current += step
          escaped = true
        }
      }
      else if (escaped)
        current += step
      else if (content(current) == '.') {
        if (!separator) {
          separator = true
          current += step
        }
        else
          current = 0
      }
      else if (Character.isJavaIdentifierPart(content(current))) {
        if (current == point || separator) {
          current += step
          point = current
          separator = false
        }
        else
          current = 0
      }
      else if (Character.isWhitespace(content(current)))
        current += step
      else
        current = 0

    point
  }
}
