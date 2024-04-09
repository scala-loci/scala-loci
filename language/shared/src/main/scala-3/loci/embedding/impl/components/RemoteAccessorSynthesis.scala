package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.util.control.NonFatal

@experimental
trait RemoteAccessorSynthesis:
  this: Component & Commons & Placements & Annotations =>
  import quotes.reflect.*

  private def encodeName(name: String) = name flatMap:
    case '~' => "$tilde"
    case '=' => "$eq"
    case '<' => "$less"
    case '>' => "$greater"
    case '!' => "$bang"
    case '#' => "$hash"
    case '%' => "$percent"
    case '^' => "$up"
    case '&' => "$amp"
    case '|' => "$bar"
    case '*' => "$times"
    case '/' => "$div"
    case '+' => "$plus"
    case '-' => "$minus"
    case ':' => "$colon"
    case '?' => "$qmark"
    case '@' => "$at"
    case '\\' => "$bslash"
    case c => if !Character.isJavaIdentifierPart(c) then f"$$u${c.toInt}%04X" else c.toString

  private def classFileName(symbol: Symbol) =
    constructFullName(symbol,
      name = symbol => encodeName(targetName(symbol)),
      separator = symbol => if symbol.isPackageDef then "." else "$",
      skip = _.isPackageObject)

  private def mangledSymbolName(symbol: Symbol) =
    f"loci$$${s"${implementationForm(symbol)} ${fullName(symbol)}".hashCode}%08x"

  private def implementationForm(symbol: Symbol) =
    if symbol.flags is Flags.Module then "object"
    else if symbol.flags is Flags.Trait then "trait"
    else "class"

  private def accessorSignature(name: String, params: List[List[Symbol]], result: TypeRepr) =
    val paramsSignature = params flatMap: params =>
      if params.isEmpty || params.head.isTerm then
        TypeToken.`(` :: ((params flatMap { param => TypeToken.`,` ++ TypeToken.typeSignature(param.info) }).drop(2) :+ TypeToken.`)`)
      else
        List.empty
    val signature =
      TypeToken(name) :: paramsSignature ++ TypeToken.`:` ++ TypeToken.typeSignature(result)
    TypeToken.serialize(signature)

  private val unknownTransmittableSignature = "########"

  private val abstractTransmittableSignature = "abstract"

  private def transmittableSignature(term: Term) =
    TermToken.serializeTerm(term).toOption.fold(unknownTransmittableSignature): term =>
      f"${term.hashCode}%08x"

  def synthesizeAccessors(module: Symbol): List[Definition] =
    val tree =
      try module.tree
      catch case NonFatal(_) => Literal(NullConstant())

    tree match
      case tree: ClassDef => synthesizeAccessorsFromTree(tree)
      case _ => synthesizeAccessorsFromClass(classFileName(tree.symbol))
  end synthesizeAccessors

  private def synthesizeAccessorsFromTree(tree: ClassDef): List[Definition] =
    object accessCollector extends TreeAccumulator[(String, Int, List[Either[MethodType, Symbol]], List[Term])]:
      override def foldTree(accesses: (String, Int, List[Either[MethodType, Symbol]], List[Term]), tree: Tree)(owner: Symbol) =
        val (signature, index, blocks, transmittables) = accesses
        tree match
          case _ => foldOverTree(accesses, tree)(owner)

    val (definitions, transmittables) =
      tree.body.foldLeft(List.empty[Either[MethodType, Symbol]], List.empty[Term]):
        case ((blocks, transmittables), stat @ (_: ValDef | _: DefDef))
            if (stat.symbol.isField || stat.symbol.isMethod) && !stat.symbol.isModuleDef =>
          val tpe = stat match
            case stat: ValDef => stat.tpt.tpe
            case stat: DefDef => stat.returnTpt.tpe

          PlacementInfo(tpe) collect:
            case placementInfo if !placementInfo.modality.local =>
              val params =
                stat.symbol.paramSymss map: params =>
                  if params.isEmpty || params.head.isTerm then
                    s"(${(params map { _.info.safeShow(Printer.SafeTypeReprShortCode) }).mkString(", ")})"
                  else
                    s"[${(params map { _.name }).mkString(", ")}]"
              val signature = s"${stat.symbol.name}${params.mkString}: ${placementInfo.valueType.safeShow(Printer.SafeTypeReprShortCode)}"

              accessCollector.foldTree((signature, 0, Right(stat.symbol) :: blocks, transmittables), stat)(stat.symbol) match
                case (_, _, blocks, transmittables) => (blocks, transmittables)

          (blocks, transmittables)

        case (accesses, _) =>
          accesses

    List.empty
  end synthesizeAccessorsFromTree

  private def synthesizeAccessorsFromClass(name: String): List[Definition] =
    List.empty
  end synthesizeAccessorsFromClass
end RemoteAccessorSynthesis
