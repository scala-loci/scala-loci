package loci
package language
package transmitter

import loci.transmitter.RemoteRef

import scala.language.experimental.macros
import scala.language.implicitConversions

trait RemoteReference { this: Remote.type =>
  trait Reference[+P] extends Remote[P] with RemoteRef {
    def asRemote[R]: Option[Remote[R]] =
      macro embedding.impl.Remote.asRemote[R]

    def asReference: Reference[P]
    def authenticated: Boolean
    def authenticate(): Unit
  }

  implicit def reference[P](remote: Remote[P]): Remote.Reference[P] = remote match {
    case remote: Remote.Reference[P] => remote
    case _ => throw new runtime.PeerImplementationError
  }
}
