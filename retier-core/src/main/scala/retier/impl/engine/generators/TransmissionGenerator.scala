package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait TransmissionGenerator { this: Generation =>
  val c: Context
  import c.universe._
  import trees._

  val generateTransmissions = UniformAggregation[PlacedStatement] {
      aggregator =>

    val stats = aggregator.all[PlacedStatement] map { stat =>
      stat.copy(expr = new TransmissionGenerator(stat) transform stat.expr)
    }

    echo(
      verbose = true,
      s"Generated transmissions for placed expressions " +
      s"(${stats.size} placed statements generated, existing replaced)")

    aggregator replace stats
  }

  private class TransmissionGenerator(stat: PlacedStatement)
      extends Transformer {
    override def transform(tree: Tree) = tree match {
      case tree @ q"$_[..$tpts](...$exprss)"
          if symbols.transmit contains tree.symbol =>
        val Seq(Seq(value), Seq(_, transmissionProvider)) = exprss

        val Seq(_, remoteName, localName) =
          transmissionProvider.tpe.typeArgs.head.typeArgs map {
            _.typeSymbol.name.toTermName
          }

        val peerName = retierName(stat.peerType.typeSymbol.asType.name)

        val (enclosingName, abstraction) = value match {
          case q"$tpname.this.$tname[..$tpts](...$exprss)" =>
            val abstractionName = retierName(tname)
            (tpname, q"$peerName.this.$abstractionName[..$tpts](...$exprss)")

          case _ =>
            c.abort(value.pos,
              "identifier of same scope, selected remote value, " +
              "or remote expression expected")
        }

        val createTransmission = TermName(tree.symbol match {
          case symbols.transmitMultiple => "createMultipleTransmission"
          case symbols.transmitOptional => "createOptionalTransmission"
          case symbols.transmitSingle => "createSingleTransmission"
        })

        val remote = q"$enclosingName.this.$remoteName.peerTypeTag"
        val local = q"$enclosingName.this.$localName.peerTypeTag"
        val system = retierTermName("system")

        q"$system.$createTransmission($abstraction)($remote, $local)"

      case _ if tree.tpe <:< types.transmissionProvider =>
        c.abort(tree.pos, "unexpected value of type TransmissionProvider")

      case _ =>
        super.transform(tree)
    }
  }
}
