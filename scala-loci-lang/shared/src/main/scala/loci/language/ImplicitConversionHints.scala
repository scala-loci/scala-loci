package loci
package language

import scala.collection.mutable
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

object implicitConversionHints {
  private implicit object stringOrdering extends Ordering[String] {
    override def compare(x: String, y: String): Int = {
      val length = math.min(x.length, y.length)
      var i = 0

      while (i < length) {
        val xi = x(i)
        val yi = y(i)

        if (Character.isLowerCase(xi) && Character.isUpperCase(yi))
          return -1
        if (Character.isUpperCase(xi) && Character.isLowerCase(yi))
          return 1
        if (xi != yi)
          return xi - yi

        i += 1
      }

      x.length - y.length
    }
  }

  private class AnnotationPeek[C <: blackbox.Context](val c: C) {
    import c.universe._

    private val cache = mutable.Map.empty[scala.reflect.internal.util.SourceFile, Option[Tree]]

    def couldHaveAnnotations(symbol: Symbol) = {
      val source = symbol.pos.source

      val tree = cache.getOrElseUpdate(source, {
        try Some(c.parse(erasePackageStatements(source.content)))
        catch { case _: reflect.macros.ParseException => None }
      })

      tree.fold(true) {
        _ exists {
          case tree: ImplDef if tree.mods.annotations.nonEmpty =>
            val positions = tree collect { case tree if tree.pos != NoPosition => tree.pos }
            val min = positions minBy { pos => math.min(pos.point, pos.start) }
            val max = positions maxBy { pos => math.max(pos.point, pos.end) }

            symbol.pos.point >= math.min(min.point, min.start) &&
            symbol.pos.point <= math.max(max.point, max.end)

          case _ =>
            false
        }
      }
    }

    private def erasePackageStatements(content: Array[Char]) = {
      val builder = new mutable.StringBuilder(content.length)
      val length = content.length - 7
      var i = 0

      while (i < length)
        if (content(i) == 'p' &&
            content(i + 1) == 'a' &&
            content(i + 2) == 'c' &&
            content(i + 3) == 'k' &&
            content(i + 4) == 'a' &&
            content(i + 5) == 'g' &&
            content(i + 6) == 'e' &&
            !Character.isJavaIdentifierPart(content(i + 7)) &&
            (i == 0 || !Character.isJavaIdentifierPart(content(i - 1)))) {
          builder.append("       ")
          i += 7
        }
        else {
          builder.append(content(i))
          i += 1
        }

      while (i < length + 7) {
        builder += content(i)
        i += 1
      }

      builder.toString
    }
  }

  private def AnnotationPeek(c: blackbox.Context): AnnotationPeek[c.type] =
    new AnnotationPeek(c)

  private def findImplicits(c: blackbox.Context) = {
    import c.universe._

    val annotationPeek = AnnotationPeek(c)

    val implicitConversions = mutable.ListBuffer.empty[(MethodSymbol, List[(Symbol, Symbol)])]
    val implicitValues = mutable.ListBuffer.empty[(MethodSymbol, List[(Symbol, Symbol)])]
    val visited = mutable.Set.empty[Symbol]

    val rawInfoMethod =
      try Some(Class.forName(s"scala.reflect.internal.Symbols$$Symbol").getMethod("rawInfo"))
      catch { case _: ClassNotFoundException | _: NoSuchMethodException => None }

    def rawInfo(symbol: Symbol) =
      try rawInfoMethod.fold(NoType) {
        _.invoke(symbol) match {
          case tpe: Type => tpe
          case _ => NoType
        }
      }
      catch { case _: IllegalArgumentException => NoType }

    def collectImplicits(symbol: Symbol, path: List[(Symbol, Symbol)]): Unit =
      if (visited.add(symbol) && (symbol.isPackage || !symbol.isJava)) {
        val info =
          try
            if (symbol.pos == NoPosition || !annotationPeek.couldHaveAnnotations(symbol))
              symbol.info
            else
              rawInfo(symbol)
          catch { case NonFatal(_) => NoType }

        info.members foreach { member =>
          val root = symbol == c.mirror.RootClass
          val knownPackage =
            root && (member.name.toString match {
              case "java" | "javax" | "jdk" | "netscape" | "sun" => true
              case _ => false
            })

          if (!knownPackage && member.isTerm && member.isPublic) {
            val currentPath = if (root) path else symbol -> member.owner :: path

            if (member.asTerm.isStable)
              collectImplicits(
                if (member.isModule) member.asModule.moduleClass else member,
                currentPath)

            if (member.isMethod &&
                member.isImplicit &&
                symbol != definitions.ScalaPackageClass &&
                symbol != definitions.PredefModule.moduleClass) {
              val method = member.asMethod
              val tpe = method.info.finalResultType
              if (!(tpe =:= definitions.NothingTpe || tpe =:= definitions.NullTpe))
                method.paramLists match {
                  case List() =>
                    implicitValues += method -> currentPath
                  case List(arg :: _) if arg.isImplicit =>
                    implicitValues += method -> currentPath
                  case List(List(conversion)) if !conversion.isImplicit =>
                    implicitConversions += method -> currentPath
                  case List(List(conversion), arg :: _) if !conversion.isImplicit && arg.isImplicit =>
                    implicitConversions += method -> currentPath
                  case _ =>
                }
            }
          }
        }
      }

    noReporting(c.universe) {
      collectImplicits(c.mirror.RootClass, List.empty)
    }

    implicitValues.toList -> implicitConversions.toList
  }

  def apply(c: blackbox.Context)(tpe: c.Type): String = {
    import c.universe._

    val (implicitValues, implicitConversions) = findImplicits(c)

    def finalResultType(symbol: Symbol, path: List[(Symbol, Symbol)]) =
      path.foldLeft(symbol.info.finalResultType) { case (tpe, (stable, owner)) =>
        val info = if (stable.isType) stable.asType.toType else stable.info.finalResultType
        tpe.asSeenFrom(info, owner)
      }

    def baseCompanions(symbol: Symbol) =
      if (symbol.isClass)
        symbol.asClass.baseClasses.distinct map { symbol =>
          val companion = symbol.companion
          if (companion.isModule) companion.asModule.moduleClass else companion
        }
      else
        List(symbol.companion)

    def importStatement(path: List[(Symbol, Symbol)]) =
      path.size -> (path.reverseIterator map { case (stable, _) => stable.name.decodedName.toString }).mkString("import ", ".", "._")

    def relatedImportStatements(symbol: MethodSymbol, path: List[(Symbol, Symbol)]) = {
      val visited = mutable.Set.empty[(MethodSymbol, List[(Symbol, Symbol)])]

      def relatedImportStatements(symbol: MethodSymbol, path: List[(Symbol, Symbol)]): List[(Int, String)] =
        if (visited.add(symbol -> path))
          symbol.paramLists.lastOption.toList flatMap {
            _ flatMap { arg =>
              val tpe = finalResultType(arg, path)
              if (!(tpe =:= definitions.AnyTpe ||
                    tpe =:= definitions.AnyValTpe ||
                    tpe =:= definitions.AnyRefTpe ||
                    tpe =:= definitions.ObjectTpe ||
                    tpe =:= definitions.NothingTpe ||
                    tpe =:= definitions.NullTpe))
                (implicitValues collect {
                  case (symbol, path @ (stable, _) :: _)
                      if symbol.info.finalResultType.typeSymbol == tpe.typeSymbol =>
                    val related = relatedImportStatements(symbol, path)
                    val companions = baseCompanions(symbol.info.finalResultType.dealias.typeSymbol)

                    if (!(companions contains stable))
                      importStatement(path) :: related
                    else
                      related
                }).flatten
              else
                List.empty
            }
          }
        else
          List.empty

      relatedImportStatements(symbol, path)
    }

    val companions = baseCompanions(tpe.dealias.typeSymbol)

    val hint =
      ((implicitConversions
        collect { case (symbol, path @ (stable, _) :: _) if symbol.paramLists.head.head.info <:< tpe =>
          (symbol,
           path,
           if (!(companions contains stable)) importStatement(path) else 0 -> "")
        }
        groupBy { case (_, _, importStatement) => importStatement }).toList
        sortBy { case (importStatement, _) => importStatement }
        map { case ((_, importStatement), entries) =>
          val typedEntries =
            entries filter { case (symbol, _, _) =>
              c.typecheck(q"$symbol(${c.prefix.tree})", silent = true).nonEmpty
            }

          val selectedEntries =
            if (typedEntries.nonEmpty) typedEntries else entries

          val identifiers = (selectedEntries flatMap { case (symbol, _, _) =>
            symbol.info.finalResultType.members collect {
              case symbol
                if !symbol.isConstructor &&
                   !symbol.isImplicit &&
                   symbol.isTerm &&
                   symbol.isPublic &&
                   symbol.owner != definitions.AnyClass &&
                   symbol.owner != definitions.AnyValClass &&
                   symbol.owner != definitions.AnyRefClass &&
                   symbol.owner != definitions.ProductClass &&
                   symbol.owner != definitions.ObjectClass =>
                symbol.name.decodedName.toString
            }
          }).distinct.sorted

          if (identifiers.nonEmpty) {
            val importStatements =
              (selectedEntries flatMap { case (symbol, path, _) =>
                relatedImportStatements(symbol, path)
              }).distinct.sorted map { case (_, importStatement) =>
                importStatement
              }

            val related = importStatements filterNot { _ == importStatement }

            val relatedHint =
              if (related.nonEmpty) s"\n${related map { related => s"  (related `$related`)" } mkString "\n"}" else ""

            val importHint =
              if (importStatement.nonEmpty) s"\n  (via `$importStatement`)" else ""

            s"\n\nHint: You may use one of the following:\n  ${identifiers mkString ", "}$importHint$relatedHint"
          }
          else
            ""
        }).mkString

    if (hint.nonEmpty) s"$hint\n " else hint
  }
}
