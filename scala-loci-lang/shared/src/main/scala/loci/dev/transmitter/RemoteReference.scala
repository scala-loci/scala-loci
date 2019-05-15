package loci.dev
package transmitter

import scala.language.experimental.macros
import scala.language.implicitConversions

trait RemoteReference { this: Remote.type =>
  trait Reference[+P] extends loci.dev.Remote[P] with loci.transmitter.RemoteRef {
    def asReference: Reference[P]
    def asRemote[R]: Option[loci.dev.Remote[R]] = macro language.impl.Remote.asRemote[R]
    def authenticated: Boolean
    def authenticate(): Unit
  }

  implicit def reference[P](remote: loci.dev.Remote[P]): Remote.Reference[P] = remote match {
    case remote: Remote.Reference[P] => remote
    case _ => throw new Exception("invalid remote reference implementation: " +
      (if (remote == null) "null" else remote.getClass.getName))
  }
}
