package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait SetupExpressionProcessor { this: Generation =>
  val c: Context
  import c.universe._

  def createSetupExpression(peerTree: Tree, peerType: Type,
      peerSymbols: List[TypeSymbol]) = {
    val implementation = peerImplementationTree(peerTree, peerType, peerSymbols)
    val typeTag = peerTypeTagTree(peerTree, peerType, peerSymbols)

    import trees._
    import names._

    markRetierSynthetic(
      q"""{
        val peer = new $peerTree
        $Runtime.run(
          peer.$connection,
          peer,
          $typeTag.${names.peerType},
          (context, connections, remotes) => new $implementation {
            override lazy val $system =
              new $System(context, connections, remotes, this)
          }.$system)
      }""",
      peerTree.pos)
  }
}
