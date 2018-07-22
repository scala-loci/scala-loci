package loci.dev
package language
package impl

import scala.reflect.macros.blackbox.Context

trait Peers {
  val c: Context

  import c.universe._

  sealed trait Tie

  object Tie {
    val Multiple = new Tie { override def toString: String = "Multiple" }
    val Optional = new Tie { override def toString: String = "Optional" }
    val Single = new Tie { override def toString: String = "Single" }
  }

  case class Peer(symbol: Symbol, name: TypeName, bases: List[Symbol], ties: List[(Tie, Symbol)])

  object Peers {
    val MultipleTpe = typeOf[Multiple[_]]
    val OptionalTpe = typeOf[Optional[_]]
    val SingleTpe = typeOf[Single[_]]
    val PeerAnnotationTpe = typeOf[peer]
  }

  class Peers(underExpansion: Set[Symbol]) {
    def requirePeerType(symbol: Symbol, pos: Position): Peer = {
      val (symbolPos, symbolName) = info(symbol, pos)

      val peer = checkPeerType(symbol, pos) getOrElse c.abort(symbolPos,
        s"$symbolName is not a peer type: @peer type ${symbol.name}")

      if (!(underExpansion contains symbol) && (symbol.owner.info member peer.name) == NoSymbol)
        c.abort(symbolPos,
          s"no generated peer definition found for peer type $symbolName, " +
          s"maybe ${symbol.owner.fullName} is not multitier: @multitier ${symbol.owner}")

      peer
    }

    def checkPeerType(symbol: Symbol, pos: Position): Option[Peer] = {
      // force loading of annotations
      val symbolType = symbol.info

      if (symbol.annotations exists { _.tree.tpe <:< Peers.PeerAnnotationTpe })
        Some(cache.getOrElse(symbol, {
          val (symbolPos, symbolName) = info(symbol, pos)

          if (symbol.annotations.size != 1)
            c.abort(symbolPos,
              s"peer types cannot have annotations: $symbolName")

          // destruct base peers and tie specification
          val (basesSpec, tiesSpec) =
            typeByUpperBound(symbolType, symbolPos, "peer types", symbolName) match {
              case RefinedType(peerParents, peerDecls) =>
                if (peerDecls.size == 1 && peerDecls.head.name == TypeName("Tie")) {
                  val tieSymbol = peerDecls.head
                  val tieType = tieSymbol.info
                  val tiePos = if (tieSymbol.pos.isEmpty) pos else tieSymbol.pos

                  typeByUpperBound(tieType, tiePos, "tie specifications", symbolName) match {
                    case RefinedType(tieParents, tieDecls) =>
                      if (tieDecls.nonEmpty)
                        c.abort(tiePos,
                          s"tie specifications cannot have members: peer type $symbolName")
                      peerParents -> tieParents
                    case tpe =>
                      peerParents -> List(tpe)
                  }
                }
                else if (peerDecls.isEmpty)
                  peerParents -> List.empty
                else
                  c.abort(symbolPos,
                    s"peers can only have a single type member { type Tie }: $symbolName")

              case tpe =>
                List(tpe) -> List.empty
            }

          // ensure all base peers are peer types
          // and ignore base `Any` and `AnyRef`
          val bases = basesSpec collect {
            case base if !(base =:= definitions.AnyTpe || base =:= definitions.AnyRefTpe) =>
              val baseSymbol = base.typeSymbol
              requirePeerType(baseSymbol, pos)
              baseSymbol
          }

          // ensure ties are specified to be `Multiple`, `Optional` or `Single`
          // and ignore tie `Any` and `AnyRef`
          val ties = tiesSpec collect {
            case tie if !(tie =:= definitions.AnyTpe || tie =:= definitions.AnyRefTpe) =>
              val multiplicity =
                if (tie <:< Peers.MultipleTpe) Tie.Multiple
                else if (tie <:< Peers.OptionalTpe) Tie.Optional
                else if (tie <:< Peers.SingleTpe) Tie.Single
                else c.abort(symbolPos, s"illegal tie specification: $tie")

              multiplicity -> tie.typeArgs.head.typeSymbol
          }

          // construct peer and add it to the cache
          // so we do not run checks again
          val name = TypeName(s"$$loci$$peer$$${symbol.name}")
          val peer = Peer(symbol, name, bases, ties)
          cache += symbol -> peer

          // ensure that tied types are peer types
          ties foreach { case (_, tieSymbol) =>
            requirePeerType(tieSymbol, pos)
          }

          peer
        }))
      else
        None
    }

    private def typeByUpperBound(
        tpe: Type, pos: Position,
        constructs: String, name: String): Type = tpe match {
      case TypeBounds(low, high) =>
        if (low =:= definitions.NothingTpe)
          high
        else
          c.abort(pos, s"$constructs cannot have lower type bounds: $name")
      case _ =>
        c.abort(pos, s"$constructs cannot be type aliases: $name")
    }

    private def info(symbol: Symbol, pos: Position): (Position, String) = {
      val symbolPos = if (symbol.pos.isEmpty) pos else symbol.pos
      val name = symbol.fullName
      val index = name lastIndexOf "."
      val symbolName =
        if (index > 0) s"${name.substring(index + 1)} in ${name.substring(0, index)}"
        else name
      symbolPos -> symbolName
    }

    private val cache = collection.mutable.Map.empty[Symbol, Peer]
  }
}
