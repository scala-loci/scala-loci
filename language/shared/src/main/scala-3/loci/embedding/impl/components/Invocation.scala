package loci
package embedding
package impl
package components

import utility.reflectionExtensions.*

import scala.annotation.experimental
import scala.collection.mutable

@experimental
trait Invocation:
  this: Component & Commons & Peers & Synthesis & Access=>
  import quotes.reflect.*

  def rewireInvocations(module: ClassDef): ClassDef =

    object stuff extends SafeTreeMap(quotes):
      private val modules = mutable.Stack.empty[Symbol]
      private val peers = mutable.Stack.empty[Symbol]
      private val placedValues = mutable.Stack.empty[Map[Symbol, Symbol]]

      extension [S](stack: mutable.Stack[S]) private def runStacked[T](value: Option[S])(body: => T) =
        value.fold(body): value =>
          stack.push(value)
          val result = body
          stack.pop()
          result

      override def transformTerm(term: Term)(owner: Symbol) =
        val module = modules.head
        val peer = peers.headOption getOrElse defn.AnyClass

        super.transformTerm(term)(owner)
      end transformTerm

      override def transformStatement(stat: Statement)(owner: Symbol) = stat match
        case stat: ClassDef =>
          val symbol = stat.symbol
          val peer = placedValues.headOption flatMap { _.get(symbol) } getOrElse defn.AnyClass

          val definitions =
            if isMultitierModule(symbol) then
              PeerInfo.ofModule(symbol) map: peerInfo =>
                val peer = peerInfo.peerType.typeSymbol
                placedValuesSymbol(symbol, peer) -> peer
            else
              List.empty

          placedValues.runStacked(Some(definitions.toMap)):
            peers.runStacked(Some(peer)):
              modules.runStacked(Option.when(isMultitierNestedPath(symbol)) { symbol }):
                super.transformStatement(stat)(owner)

        case _ =>
          placedValues.runStacked(Some(Map.empty)):
            super.transformStatement(stat)(owner)
      end transformStatement
    end stuff

    val classDef @ ClassDef(_, _, _, _, _) = stuff.transformStatement(module)(module.symbol.owner): @unchecked
    classDef
  end rewireInvocations
end Invocation
