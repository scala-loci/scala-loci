package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait NamesValidator { this: Generation =>
  val c: Context
  import c.universe._

  val validateNames = UniformAggregation[
    EnclosingContext with InputStatement with PeerDefinition] {
      aggregator =>

    echo(verbose = true, " Validating identifier names")

    aggregator.all[InputStatement] foreach {
      _.stat foreach {
        case tree: DefTree if tree.name.isRetierName =>
          c.abort(tree.pos,
            "identifier name not allowed in `multitier` environment")
        case _ =>
      }
    }

    aggregator.all[InputStatement] foreach {
      _.stat foreach {
        case tree: ImplDef =>
          tree.impl.parents foreach { parent =>
            parent.tpe.members foreach { member =>
              if (member.name.isRetierName)
                c.abort(parent.pos,
                  "identifier name not allowed in `multitier` environment: " +
                  member.name)
            }
          }
        case _ =>
      }
    }

    aggregator.all[EnclosingContext].head.bases foreach { base =>
      base.tpe.members foreach { member =>
        if (member.name.isRetierName)
          c.abort(base.pos,
            "identifier name not allowed in `multitier` environment: " +
            member.name)
      }
    }

    echo(verbose = true, "  [identifier names validated]")

    aggregator
  }
}
