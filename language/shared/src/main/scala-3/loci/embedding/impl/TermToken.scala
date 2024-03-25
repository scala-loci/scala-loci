package loci
package embedding
package impl

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.util.control.NonFatal
import scala.quoted.*

@experimental
object TermToken:
  def serializeTerm(using Quotes)(term: quotes.reflect.Term): Either[String, String] =
    import quotes.reflect.*

    val builder = StringBuilder()
    var unexpectedTreeEncountered = false
    var printingParentsOrAnnotations = false
    var printingParentsOrAnnotationsNeedParens = false

    val repeated = Symbol.requiredClass("scala.<repeated>")
    val forceInline = Symbol.requiredClass("scala.forceInline")
    val internal = Symbol.requiredPackage("scala.annotation.internal")

    inline def escape(string: String) = string flatMap:
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"' => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case c => if c.isControl then f"\\u${c.toInt}%04x" else c.toString

    def printSymbol(symbol: Symbol): Unit =
      val name = (if symbol.isClassDef && symbol.isModuleDef then symbol.companionModule else symbol).name
      if symbol.isPackageObject then
        printSymbol(symbol.owner)
      else if symbol.maybeOwner == defn.RootClass then
        builder ++= name
      else if symbol.maybeOwner.isClassDef && !symbol.maybeOwner.isPackageDef && !symbol.maybeOwner.isModuleDef then
        printSymbol(symbol.maybeOwner)
        builder += '#' ++= name
      else if symbol.maybeOwner.exists then
        printSymbol(symbol.maybeOwner)
        builder += '.' ++= name
      else if symbol.exists then
        builder ++= name

    inline def printType(tpe: TypeRepr): Unit =
      builder ++= (TypeToken.typeSignature(tpe) flatMap { _.token }).mkString

    inline def printSingletonTypeTermRefOrElse(tree: Tree)(fallback: => Unit): Unit = tree match
      case tree: Term => tree.tpe match
        case TermRef(qualifier, name) if qualifier.typeSymbol == defn.RootClass =>
          builder ++= name
        case TermRef(NoPrefix(), name) =>
          builder ++= name
        case TermRef(qualifier @ TermRef(_, _), name) =>
          printType(qualifier)
          builder += '.' ++= name
        case TermRef(qualifier, name) if qualifier.typeSymbol.isModuleDef || qualifier.typeSymbol.isPackageDef =>
          printType(qualifier)
          builder += '.' ++= name
        case TermRef(qualifier, name) =>
          printType(qualifier)
          builder += '#' ++= name
        case _ =>
          fallback
      case _ =>
        fallback

    inline def printTrees(trees: List[Tree], start: String, separator: String, end: String): Unit =
      builder ++= start
      if trees.nonEmpty then
        printingParentsOrAnnotationsNeedParens = false
        printTree(trees.head)
        trees.tail foreach: tree =>
          builder ++= separator
          printingParentsOrAnnotationsNeedParens = false
          printTree(tree)
      builder ++= end

    inline def printNestedTree(tree: Tree): Unit = tree match
      case _: If | _: Match | _: While | _: Try | _: Return =>
        builder += '('
        printTree(tree)
        builder += ')'
      case _ =>
        printTree(tree)

    def printTree(tree: Tree): Unit =
      try tree match
        case Wildcard() =>
          builder += '_'
        case TypedOrTest(tree, tpt) =>
          printNestedTree(tree)
          if tpt.tpe.typeSymbol == repeated then
            tree match
              case Repeated(_, _) | Inlined(None, Nil, Repeated(_, _)) =>
              case _ => builder ++= "*"
        case Bind(name, Wildcard()) =>
          builder ++= name
        case Bind(name, Typed(Wildcard(), tpt)) =>
          builder ++= name ++= ": "
          printTree(tpt)
        case Bind(name, pattern) =>
          builder ++= name ++= " @ "
          printTree(pattern)
        case Unapply(fun, implicits, patterns) =>
          val extractor = fun match
            case TypeApply(fun, _) => fun
            case _ => fun
          extractor match
            case Select(extractor, "unapply" | "unapplySeq") => printTree(extractor)
            case Ident("unapply" | "unapplySeq") => printSymbol(fun.symbol.owner)
            case _ => printTree(extractor)
          printTrees(patterns, "(", ", ", ")")
          if implicits.nonEmpty then
            printTrees(patterns, "(using ", ", ", ")")
        case Alternatives(patterns) =>
          printTrees(patterns, "(", " | ", ")")
        case CaseDef(pattern, guard, rhs) =>
          builder ++= "case "
          printTree(pattern)
          guard foreach: guard =>
            builder ++= " if "
            printNestedTree(guard)
          builder ++= " => "
          rhs match
            case Block(stats, expr) => printTrees(stats :+ expr, "", "; ", "")
            case _ => printTree(rhs)
        case Match(selector, cases) =>
          printNestedTree(selector)
          printTrees(cases, " match { ", " ", " }")
        case SummonFrom(cases) =>
          printTrees(cases, "summonFrom { ", " ", " }")
        case If(cond, thenp, elsep)  =>
          builder ++= "if "
          printNestedTree(cond)
          builder ++= " then "
          printNestedTree(thenp)
          builder ++= " else "
          printNestedTree(elsep)
        case Inlined(_, List(), expansion) =>
          printTree(expansion)
        case Inlined(_, bindings, expansion) =>
          printTrees(bindings :+ expansion, "{ ", "; ", " }")
        case Block(stats, expr) =>
          val statements = stats filter:
            case stat: ValDef => !(stat.symbol.flags is Flags.Lazy) || !(stat.symbol.flags is Flags.Module)
            case _ => true
          printTrees(statements :+ expr, "{ ", "; ", " }")
        case TypeApply(fun, _) =>
          printTree(fun)
        case Select(qualifier @ Ident("_root_"), name) if !qualifier.symbol.maybeOwner.exists =>
          builder ++= name
        case Select(qualifier, name) =>
          if !tree.symbol.isPackageObject && name != "<init>" then
            printSingletonTypeTermRefOrElse(tree):
              printNestedTree(qualifier)
              builder += '.' ++= name
          else
            printNestedTree(qualifier)
        case Ident(_) =>
          printSingletonTypeTermRefOrElse(tree):
            printSymbol(tree.symbol)
        case This(_) =>
          printSymbol(tree.symbol)
          builder ++= ".this"
        case Super(qualifier, id) =>
          printSymbol(qualifier.symbol)
          builder ++= ".super"
          id foreach: id =>
            builder += '[' ++= id += ']'
        case Apply(fun @ Ident("throw"), List(arg)) if fun.symbol.owner.name == "<special-ops>" =>
          builder ++= "throw "
          printTree(arg)
        case Apply(Select(This(_), "<init>"), args) =>
          printTrees(args, "this(", ", ", ")")
        case Apply(fun @ Select(qualifier, "apply"), args) if qualifier.tpe.isContextFunctionType =>
          printNestedTree(fun)
          printTrees(args, "(using ", ", ", ")")
        case Apply(fun, args) =>
          val needParens = printingParentsOrAnnotationsNeedParens || args.nonEmpty || !printingParentsOrAnnotations
          printingParentsOrAnnotationsNeedParens = needParens
          printNestedTree(fun)
          if needParens then
            val rememberPrintingParentsOrAnnotations = printingParentsOrAnnotations
            printingParentsOrAnnotations = false
            fun.tpe match
              case tpe: MethodType if tpe.isImplicit => printTrees(args, "(using ", ", ", ")")
              case _ => printTrees(args, "(", ", ", ")")
            printingParentsOrAnnotations = rememberPrintingParentsOrAnnotations
        case New(tpt) =>
          if !printingParentsOrAnnotations then
            builder ++= "new "
          printTree(tpt)
        case NamedArg(name, arg) =>
          builder ++= name ++= " = "
          printTree(arg)
        case Assign(lhs, rhs) =>
          printTree(lhs)
          builder ++= " = "
          printTree(rhs)
        case Closure(meth, _) =>
          printTree(meth)
        case Try(body, cases, finalizer) =>
          builder ++= "try "
          printTree(body)
          if cases.nonEmpty then
            printTrees(cases, " catch { ", " ", " }")
          finalizer foreach: finalizer =>
            builder ++= " finally "
            printTree(finalizer)
        case Return(expr, _) =>
          builder ++= "return "
          printTree(expr)
        case Repeated(elems, _) =>
          printTrees(elems, "", ", ", "")
        case While(cond, body) =>
          builder ++= "while "
          printTree(cond)
          builder ++= " do "
          printTree(body)
        case Literal(ClassOfConstant(tpe)) =>
          printSymbol(tpe.typeSymbol)
        case Literal(UnitConstant()) =>
          builder ++= "()"
        case Literal(NullConstant()) =>
          builder ++= "null"
        case Literal(StringConstant(value)) =>
          builder += '"' ++= escape(value) += '"'
        case Literal(constant) =>
          builder ++= constant.value.toString
        case tree: TypeTree =>
          printType(tree.tpe)

        case ClassDef(_, DefDef(_, paramss, _, _), parents, self, stats) =>
          printAnnotations(tree.symbol)

          val flags = tree.symbol.flags
          if flags is Flags.Implicit then builder ++= "implicit "
          if flags is Flags.Sealed then builder ++= "sealed "
          if (flags is Flags.Final) && !(flags is Flags.Module) then builder ++= "final "
          if flags is Flags.Case then builder ++= "case "

          if flags is Flags.Module then builder ++= "object " ++= tree.symbol.companionModule.name
          else if flags is Flags.Trait then builder ++= "trait " ++= tree.symbol.name
          else if flags is Flags.Abstract then builder ++= "abstract class " ++= tree.symbol.name
          else builder ++= "class " ++= tree.symbol.name

          if !(flags is Flags.Module) then
            paramss foreach:
              case params @ TermParamClause(_) => printArguments(params)
              case _ =>

          builder ++= " extends "
          printingParentsOrAnnotations = true
          printTrees(parents, "", ", ", "")
          printingParentsOrAnnotations = false

          var hasSelfType = false
          self foreach:
            case ValDef(_, Singleton(_), _) =>
            case ValDef(name, tpt, _) =>
              hasSelfType = true
              builder ++= " { " ++= (if name == "_" then "this" else name) ++= ": "
              printTree(tpt)
              builder ++= " => "

          val statements = stats filter:
            case stat @ DefDef(name, _, _, _) =>
              val flags = stat.symbol.flags
              !(flags is Flags.Param) && !(flags is Flags.ParamAccessor) && !(flags is Flags.FieldAccessor) &&
                (!(flags is Flags.Synthetic) ||
                  (!(stat.symbol.owner.flags is Flags.Module) ||
                    name != "apply" && name != "unapply" && name != "writeReplace") &&
                  (!(stat.symbol.owner.flags is Flags.Case) ||
                    name != "copy" && name != "productElementName" && !name.matches("(_|copy\\$default\\$)[1-9][0-9]*")))
            case stat @ ValDef(_, _, _) =>
              val flags = stat.symbol.flags
              !(flags is Flags.Param) && !(flags is Flags.ParamAccessor) && !(flags is Flags.FieldAccessor) &&
                (!(flags is Flags.Lazy) || !(flags is Flags.Module))
            case _ =>
              true

          if statements.nonEmpty then
            if !hasSelfType then
              builder ++= " { "
            printTrees(statements, "", "; ", " }")
          else if hasSelfType then
            builder ++= "}"

        case DefDef(name, paramss, tpt, rhs) =>
          printAnnotations(tree.symbol)

          val flags = tree.symbol.flags
          val constructor = name == "<init>"
          if flags is Flags.Implicit then builder ++= "implicit "
          if flags is Flags.Inline then builder ++= "inline "
          if flags is Flags.Override then builder ++= "override "
          if (flags is Flags.Final) && !(flags is Flags.Module) then builder ++= "final "
          if flags is Flags.Protected then builder ++= "protected "
          if flags is Flags.Private then builder ++= "private "

          builder ++= "def " ++= (if constructor then "this" else name)
          paramss foreach:
            case params @ TermParamClause(_) => printArguments(params)
            case _ =>

          if !constructor then
            builder ++= ": "
            printTree(tpt)

          rhs foreach: rhs =>
            builder ++= " = "
            printTree(rhs)

        case ValDef(name, tpt, rhs) =>
          printAnnotations(tree.symbol)

          val flags = tree.symbol.flags
          if flags is Flags.Implicit then builder ++= "implicit "
          if flags is Flags.Override then builder ++= "override "
          if (flags is Flags.Final) && !(flags is Flags.Module) then builder ++= "final "
          if flags is Flags.Protected then builder ++= "protected "
          if flags is Flags.Private then builder ++= "private "
          if flags is Flags.Lazy then builder ++= "lazy "

          builder ++= (if flags is Flags.Mutable then "var " else "val ") ++= name ++= ": "
          printTree(tpt)

          rhs foreach: rhs =>
            builder ++= " = "
            printTree(rhs)

        case TypeDef(_, _) =>

        case Import(_, _) =>

        case Export(expr, selectors) =>
          builder ++= "export "
          printTree(expr)
          builder ++= "."

          val multipleSelectors = selectors.sizeIs > 1

          if selectors.nonEmpty then
            if multipleSelectors then
              builder ++= "{"
            printSelector(selectors.head)
            selectors.tail foreach: selector =>
              builder ++= ", "
              printSelector(selector)
            if multipleSelectors then
              builder ++= "}"

        case _ =>
          unexpectedTreeEncountered = true
          builder ++= "<?>"

      catch
        case NonFatal(_) =>
          unexpectedTreeEncountered = true
          builder ++= "<?>"
    end printTree

    def printArgument(arg: ValDef): Unit =
      val ValDef(name, tpt, _) = arg
      builder ++= name ++= ": "
      printTree(tpt)

    def printArguments(clause: TermParamClause): Unit =
      if clause.isImplicit || clause.isGiven then
        builder ++= "(using "
      else
        builder ++= "("
      if clause.params.nonEmpty then
        printArgument(clause.params.head)
        clause.params.tail foreach: param =>
          printArgument(param)
      builder ++= ")"

    def printAnnotations(symbol: Symbol): Unit =
      val annotations = symbol.annotations.filter: term =>
        val symbol = term.tpe.typeSymbol
        symbol != forceInline && symbol.maybeOwner != internal
      if annotations.nonEmpty then
        printingParentsOrAnnotations = true
        printTrees(annotations, "@", " @", " ")
        printingParentsOrAnnotations = false

    def printSelector(selector: Selector): Unit = selector match
      case SimpleSelector("_") =>
        builder ++= "*"
      case SimpleSelector(name) =>
        builder ++= name
      case OmitSelector(name) =>
        builder ++= name ++= " as _"
      case RenameSelector(fromName, toName) =>
        builder ++= fromName ++= " as " ++= toName
      case GivenSelector(Some(bound)) =>
        builder ++= "given "
        printTree(bound)
      case GivenSelector(_) =>
        builder ++= "given"

    printTree(term)
    Either.cond(!unexpectedTreeEncountered, builder.toString, builder.toString)
  end serializeTerm
end TermToken
