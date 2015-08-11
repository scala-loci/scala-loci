package retier
package impl

import scala.reflect.ClassTag
import scala.reflect.macros.whitebox.Context
import scala.reflect.NameTransformer

private final case class PeerTypeImpl(name: String)
    extends PeerTypeImplBase {

  override def equals(other: Any) =
    other match {
      case PeerTypeImpl(otherName) => otherName == name
      case _ => false
    }

  override def hashCode: Int = name.hashCode
}

private final case class PeerTypeTagImpl[P](peerType: PeerTypeImpl)
    extends PeerTypeTagImplBase[P] {

  override def equals(other: Any) =
    other match {
      case PeerTypeTagImpl(otherPeerType) => otherPeerType == peerType
      case _ => false
    }

  override def hashCode: Int = peerType.hashCode
}

object PeerType {
  def create(name: String): PeerType =
    PeerTypeImpl(name)

  def create[P <: Peer: ClassTag](peer: P): PeerType =
    create(NameTransformer decode peer.getClass.getSimpleName)
}

object PeerTypeTag {
  def create[P](name: String): PeerTypeTag[P] =
    PeerTypeTagImpl(PeerTypeImpl(name))

  def impl[P: c.WeakTypeTag]
      (c: Context)(ev: c.Expr[ClassTag[P]]): c.Expr[PeerTypeTag[P]] = {
    import c.universe._

    val tpe = weakTypeOf[P]

    if (!(tpe <:< typeOf[Peer]))
      c.abort(c.enclosingPosition, s"$tpe is not a peer type")

    val name = tpe.typeSymbol.name.decodedName.toString

    c.Expr[PeerTypeTag[P]](
      q"_root_.retier.impl.PeerTypeTag.create[$tpe]($name)")
  }
}
