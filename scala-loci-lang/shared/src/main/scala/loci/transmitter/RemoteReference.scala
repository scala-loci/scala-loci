package loci
package transmitter

import scala.language.experimental.macros
import scala.language.implicitConversions

trait RemoteReference { this: Remote.type =>
  trait Reference[+P] extends loci.Remote[P] with RemoteRef {
    def asRemote[R]: Option[loci.Remote[R]] =
      macro language.impl.Remote.asRemote[R]

    def asReference: Reference[P]
    def authenticated: Boolean
    def authenticate(): Unit
  }

  implicit def reference[P](remote: loci.Remote[P]): Remote.Reference[P] = remote match {
    case remote: Remote.Reference[P] => remote
    case _ => throw new runtime.PeerImplementationError
  }
}
