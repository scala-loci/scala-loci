package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable

@experimental
trait Invocation:
  this: Component & Commons & ErrorReporter & Placements & Peers & AccessPath & PlacedValueSynthesis =>
  import quotes.reflect.*

  private def accessPath(term: Select, module: Symbol, peer: Symbol): Option[Term] =
    val path = multitierAccessPath(term, module, peer)
    if path.isEmpty && isMultitierNestedPath(term.qualifier.symbol) then
      errorAndCancel(
        s"Access to multitier value ${term.symbol.name} not allowed from module ${fullName(module)}",
        term.posInUserCode.endPosition)
    path

  def rewireInvocations(module: ClassDef): ClassDef =
    object invocationRewriter extends SafeTreeMap(quotes):
      private val modules = mutable.Stack.empty[Symbol]
      private val peersTypes = mutable.Stack.empty[TypeRepr]
      private val placedValues = mutable.Stack.empty[Map[Symbol, TypeRepr]]

      extension [S](stack: mutable.Stack[S]) private def runStacked[T](value: Option[S])(body: => T) =
        value.fold(body): value =>
          stack.push(value)
          val result = body
          stack.pop()
          result

      override def transformTerm(term: Term)(owner: Symbol) =
        val module = modules.head

        peersTypes.headOption.fold(super.transformTerm(term)(owner)): peerType =>
          term match
            // TODO: remote access to placed values of other peer instances

            // placed values on the same peer
            case PlacedValueReference(reference @ Select(_, _), placementInfo) =>
              if !(peerType <:< placementInfo.peerType) then
                errorAndCancel(
                  s"Access to value on peer ${placementInfo.peerType.safeShow(Printer.SafeTypeReprShortCode)} not allowed " +
                  s"from peer ${peerType.safeShow(Printer.SafeTypeReprShortCode)}",
                  term.posInUserCode.endPosition)

              accessPath(reference, module, peerType.typeSymbol) getOrElse { super.transformTerm(term)(owner) }

            // non-placed values in multitier modules
            case term @ Select(_, _) =>
              accessPath(term, module, peerType.typeSymbol) getOrElse { super.transformTerm(term)(owner) }

            case term @ Ident(_) =>
              val reference =
                if term.symbol.owner.isClassDef && term.symbol.owner.isModuleDef then
                  Some(Ref(term.symbol.owner.companionModule).select(term.symbol))
                else if term.symbol.owner.isClassDef && (module hasAncestor term.symbol.owner) then
                  Some(This(term.symbol.owner).select(term.symbol))
                else
                  None

              reference flatMap { accessPath(_, module, peerType.typeSymbol) } getOrElse { super.transformTerm(term)(owner) }

            case _ =>
              super.transformTerm(term)(owner)
      end transformTerm

      override def transformStatement(stat: Statement)(owner: Symbol) = stat match
        case stat: ClassDef =>
          val symbol = stat.symbol
          val peerType = placedValues.headOption flatMap { _.get(symbol) }

          val definitions =
            if isMultitierModule(symbol) then
              PeerInfo.ofModule(symbol) map: peerInfo =>
                synthesizedPlacedValues(symbol, peerInfo.peerType.typeSymbol).symbol -> peerInfo.peerType
            else
              List.empty

          placedValues.runStacked(Some(definitions.toMap)):
            peersTypes.runStacked(peerType):
              modules.runStacked(Option.when(isMultitierNestedPath(symbol)) { symbol }):
                super.transformStatement(stat)(owner)

        case _ =>
          placedValues.runStacked(Some(Map.empty)):
            super.transformStatement(stat)(owner)
      end transformStatement
    end invocationRewriter

    invocationRewriter.transformStatement(module)(module.symbol.owner) match
      case module: ClassDef @unchecked => module
  end rewireInvocations
end Invocation
