package loci
package language

import scala.annotation.StaticAnnotation

package object impl {
  type Peer
}

package impl {
  final class NonInstantiable extends StaticAnnotation
}
