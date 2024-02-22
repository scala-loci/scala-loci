package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.util.control.NonFatal

@experimental
trait RemoteAccess:
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

  private def guessClassName(symbol: Symbol) =
    constructFullName(symbol): symbol =>
      (encodeName(targetName(symbol)), if symbol.isPackageDef then "." else "$", symbol.isPackageObject)

  def synthesizeAccessors(module: Symbol): List[Definition] =
    val tree =
      try module.tree
      catch case NonFatal(_) => Literal(NullConstant())

    tree match
      case tree: ClassDef => synthesizeAccessorsFromTree(tree)
      case _ => synthesizeAccessorsFromClass(guessClassName(tree.symbol))
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
end RemoteAccess
