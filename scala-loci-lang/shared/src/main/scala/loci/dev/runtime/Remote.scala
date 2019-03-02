package loci.dev
package runtime

import scala.annotation.compileTimeOnly

object Remote {
  trait Reference extends loci.dev.Remote.Reference[Nothing] {
    val signature: Peer.Signature
  }

  @compileTimeOnly("Call only allowed in multitier code. Use `remote.asRemote[P]` instead.")
  def cast[P](reference: loci.dev.Remote.Reference[_]): Option[Remote[P]] = ???
}
