package retier
package impl
package engine

import scala.reflect.macros.blackbox.Context

object PlacedImplicitBridgeCreator {
  def apply[C <: Context](c: C): PlacedImplicitBridgeCreator[c.type] =
    new PlacedImplicitBridgeCreator[c.type](c)
}

class PlacedImplicitBridgeCreator[C <: Context](protected val c: C) {
  import c.universe._
  import Flag._

  case class ImportedSymbol(symbol: Symbol, importing: Type)

  case class ImportedImplicitConversion(method: MethodSymbol, importing: Type)

  def findImportedSymbols: List[ImportedSymbol] = {
    import c.mirror._

    val sourceFile = c.enclosingPosition.source
    val sourceFileContent = sourceFile.content.mkString

    // Some hacks to get the imports in the file which contains the expanding
    // macro. Since the reflection API does not provide the necessary
    // functionality, we parse out the the statements from the source code
    // string by brute force. The current approach is not fully reliable and
    // presumed to over-approximate the list of imported symbols.
    val packageStatement = """package\s+((?:[^\r\n;`]+|`[^`]+`)*)""".r
    val importStatement = """import\s+((?:[^\r\n;`]+|`[^`]+`)*)""".r
    val importExpression = """((?:[^\r\n\{\},;`]+|`[^`]+`)+)(\{(?:[^\r\n\{\};`]+|`[^`]+`)*\})?""".r
    val importSelector = """([^\r\n\{\}\s,;`]+|`[^`]+`)(?:\s+=>\s+([^\r\n\{\}\s,;`]+|`[^`]+`))""".r

    val packages =
      (packageStatement
        findAllMatchIn sourceFileContent
        map { _ group 1 }
        foldLeft List("")) { (list, name) =>
          list ++ (list map { _ + name + '.' })
        }
      .toList

    val autoImports =
      if ((c.compilerSettings contains "-Yno-predef") ||
          (c.compilerSettings contains "-Yno-imports"))
        List.empty
      else
        List("_root_.scala.Predef._")

    val imports =
      autoImports ++
      (importStatement
        findAllMatchIn sourceFileContent
        flatMap { matches =>
          importExpression findAllMatchIn (matches group 1) flatMap { matches =>
            val base = matches group 1
            val selector = Option(matches group 2)
            selector map { selector =>
              importSelector findAllMatchIn selector flatMap { matches =>
                val renamed = matches group 2
                if (renamed != "_" && renamed != "`_`")
                  Some(base + (matches group 1))
                else
                  None
              }
            } getOrElse List(base)
          }
        })
      .toList

    val staticImpls = Seq(staticClass _, staticModule _, staticPackage _)

    def symbolType(symbol: Symbol) =
      if (symbol.isType) symbol.asType.toType else symbol.typeSignature

    def lookupSymbols(fullName: String): Seq[ImportedSymbol] = {
      val fullNameSymbols = staticImpls flatMap { staticImpl =>
        try Option(staticImpl(fullName))
        catch {
          case _: ScalaReflectionException |
               _: reflect.internal.Symbols#CyclicReference => None
        }
      }

      val symbols =
        if (fullNameSymbols.isEmpty) {
          val index = fullName lastIndexOf '.'
          if (index != -1) {
            val name = fullName substring (index + 1)
            lookupSymbols(fullName substring (0, index)) flatMap {
              case ImportedSymbol(symbol, importing) if symbol.isType =>
                val importing = symbol.asType.toType
                val symbols = Seq(
                  importing member TermName(name),
                  importing member TypeName(name))

                (symbols
                  filter { symbol =>
                    symbol != NoSymbol &&
                    (!symbol.isClass || symbol.isModule || symbol.isPackage)
                  }
                  map { ImportedSymbol(_, importing)})

              case _ =>
                  Seq.empty
            }
          }
          else
            Seq.empty
        }
        else
          fullNameSymbols collect {
            case symbol if symbol.pos.source != sourceFile =>
              ImportedSymbol(symbol, symbolType(symbol))
          }

      symbols flatMap { case imported @ ImportedSymbol(symbol, importing) =>
        Seq(imported) ++
        (if (symbol.isModule)
          Seq(ImportedSymbol(symbol.asModule.moduleClass, importing))
         else
          Seq.empty)
      }
    }

    imports flatMap { importName =>
      if (importName endsWith "._") {
        packages flatMap { packageName =>
          val name = packageName + (importName substring (0, importName.size - 2))

          lookupSymbols(name) flatMap { case ImportedSymbol(symbol, _) =>
            if (symbol.isType && (symbol.pos.source != sourceFile)) {
              val importing = symbol.asType.toType
              importing.members flatMap { symbol =>
                if (symbol.isModule)
                  Seq(ImportedSymbol(symbol, importing),
                      ImportedSymbol(symbol.asModule.moduleClass, importing))
                else
                  Seq(ImportedSymbol(symbol, importing))
              }
            }
            else
              Seq.empty
          }
        }
      }
      else
        packages flatMap { packageName =>
          lookupSymbols(packageName + importName) filter {
            case ImportedSymbol(symbol, _) =>
              symbol.pos.source != sourceFile
          }
        }
    }
  }

  def findImportedImplicitConversions: List[ImportedImplicitConversion] = {
    def asImplicitConversionOption(symbol: Symbol) =
      if (symbol.isMethod && symbol.isImplicit) {
        val method = symbol.asMethod
        val paramss = method.paramLists
        if ((paramss.size == 1 && paramss.head.size == 1) ||
            (paramss.size == 2 && paramss.head.size == 1 &&
              paramss.last.head.isImplicit))
          Some(method)
        else
          None
      }
      else
        None

    findImportedSymbols flatMap {
      case ImportedSymbol(symbol, importing) =>
        if (symbol.isClass && symbol.companion.isModule &&
            !(symbol.name.toString contains '$'))
          symbol.companion.asModule.moduleClass.asType.toType.members flatMap {
            (asImplicitConversionOption(_)
              filter { conversion =>
                !(conversion.name.toString contains '$') &&
                  ((conversion.returnType contains symbol) ||
                   (conversion.paramLists.flatten exists {
                     _ typeSignatureIn importing contains symbol
                   }) ||
                   (conversion.typeParams exists { param =>
                     (param.asType.toType contains symbol) ||
                     (param typeSignatureIn importing contains symbol)
                   }))
              }
              map {
                ImportedImplicitConversion(_, importing)
              })
          }
        else
          asImplicitConversionOption(symbol) map {
            ImportedImplicitConversion(_, importing)
          }
    }
  }

  private val prefix = "$$implicit$conversion$bridge$"

  def createPlacedImplicitBridges: List[Tree] = {
    val retierType = (c.mirror staticPackage "_root_.retier").moduleClass.asType
    val scalaType = (c.mirror staticPackage "_root_.scala").moduleClass.asType

    val function1 = c.mirror staticClass "_root_.scala.Function1"

    val byname = scalaType.toType member TypeName("<byname>")

    def isInRetierPackage(tpe: Type): Boolean = {
      def isInRetierPackage(symbol: Symbol): Boolean =
        symbol == retierType ||
        (symbol.owner != NoSymbol && isInRetierPackage(symbol.owner))

      tpe exists { tpe => isInRetierPackage(tpe.typeSymbol) }
    }

    findImportedImplicitConversions flatMap {
      case ImportedImplicitConversion(method, importing) =>
        val T = TypeName(c freshName "T")
        val U = TypeName(c freshName "U")
        val ev = TermName(c freshName "ev")
        val name = TermName(c freshName s"$prefix${method.name.toString}")

        val methodType = method typeSignatureIn importing

        def uncurry(methodType: MethodType): (List[List[Symbol]], Type) =
          methodType match {
            case MethodType(params, methodType @ MethodType(_, _)) =>
              val (resultParams, resultType) = uncurry(methodType)
              (params :: resultParams, resultType)
            case MethodType(params, resultType) =>
              (List(params), resultType)
          }

        val (typeParams, (paramLists, returnType)) = methodType match {
          case PolyType(typeParams, methodType @ MethodType(_, _)) =>
            (typeParams, uncurry(methodType))
          case methodType @ MethodType(_, _) =>
            (List.empty, uncurry(methodType))
        }

        val param = paramLists.head.head
        val paramName = param.name.toTermName
        val paramType = param typeSignatureIn importing

        val typeParamsBounds =
          typeParams map { symbol =>
            def createTypeDef(mods: Modifiers, symbol: Symbol): (TypeDef, Boolean) = {
              val (typeParams, resultType, unbounded) =
                (symbol typeSignatureIn importing) match {
                  case PolyType(typeParams, resultType) =>
                    (typeParams, resultType, false)
                  case resultType @ TypeBounds(lo, hi) =>
                    (List.empty, resultType,
                     lo =:= definitions.NothingTpe && hi =:= definitions.AnyTpe)
                  case resultType =>
                    (List.empty, resultType, false)
                }

              TypeDef(
                Modifiers(Flag.PARAM),
                symbol.name.toTypeName,
                typeParams map { symbol =>
                  val (typeDef, _) = createTypeDef(Modifiers(), symbol)
                  typeDef
                },
                TypeTree(resultType)) -> unbounded
            }

            createTypeDef(Modifiers(Flag.PARAM), symbol)
          }

        val (typeParamDefs, _) = typeParamsBounds.unzip

        val unboundedTypeParams = typeParamsBounds collect {
          case (typeParam, true) => typeParam.name.toTypeName
        }


        def isUnbounded(tpe: Type): Boolean = {
          tpe =:= definitions.AnyTpe ||
          tpe =:= definitions.AnyRefTpe ||
          tpe =:= definitions.AnyValTpe ||
          (unboundedTypeParams contains tpe.typeSymbol.name.toTypeName) ||
          (((tpe.baseClasses contains function1) || tpe.typeSymbol == byname) &&
            (tpe.typeArgs exists isUnbounded))
        }


        val paramDefsBounds =
          if (paramLists.size > 1)
            paramLists.last map { symbol =>
              val tpe = symbol typeSignatureIn importing
              ValDef(
                Modifiers(Flag.PARAM),
                symbol.name.toTermName,
                TypeTree(tpe),
                EmptyTree) -> isUnbounded(tpe)
            }
          else
            List.empty

        val (paramDefs, _) = paramDefsBounds.unzip

        val hasUnboundedParam = paramDefsBounds exists {
          case (_, unbounded) => unbounded
        }


        val extraParamDef =
          q"${Flag.PARAM} val $ev: _root_.retier.LocalValueTypes[$T, $U]"

        val result =
          q"""${Flag.SYNTHETIC | Flag.ARTIFACT} implicit def $name
                [..$typeParamDefs, $T, $U <: $paramType]
                ($paramName: $T)
                (implicit ..$paramDefs, $extraParamDef)
                : $returnType = _root_.retier.`#macro`"""

        val exclude =
          hasUnboundedParam ||
          isUnbounded(paramType) ||
          isUnbounded(returnType) ||
          isInRetierPackage(paramType) ||
          isInRetierPackage(returnType) ||
          (typeParams exists { symbol =>
            isInRetierPackage(symbol typeSignatureIn importing)
          })

        if (exclude) None else Some(result)
    }
  }

  private object placedImplicitBridgesRemover extends Transformer {
    def isPlacedImplicitBridge(name: Name): Boolean =
      name.toString startsWith prefix

    def validArgCount(paramss: List[List[Tree]]) =
      paramss.size >= 1 && paramss.size <= 2 && paramss.head.size == 1

    override def transform(tree: Tree) = tree match {
      case q"$_ def $tname[..$_](...$paramss): $_ = $_"
          if validArgCount(paramss) && isPlacedImplicitBridge(tname) =>
        EmptyTree

      case q"$_.$tname[..$_](...$exprss)"
          if validArgCount(exprss) && isPlacedImplicitBridge(tname) =>
        transform(exprss.head.head)

      case q"$ident[..$_](...$exprss)" if validArgCount(exprss) =>
        ident match {
          case Ident(name) if isPlacedImplicitBridge(name) =>
            transform(exprss.head.head)
          case _ =>
            super.transform(tree)
        }

      case _ =>
        super.transform(tree)
    }
  }

  def removePlacedImplicitBridges(tree: Tree): Tree =
    placedImplicitBridgesRemover transform tree

  def removePlacedImplicitBridges(trees: List[Tree]): List[Tree] =
    trees map placedImplicitBridgesRemover.transform
}
