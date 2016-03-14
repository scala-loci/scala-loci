package retier
package impl

import RemoteRef._
import scala.language.implicitConversions

private[impl] final case class Selection[T, P <: Peer](
  props: TransmissionProperties[T], remotes: Option[Seq[Remote[P]]])
    extends SelectionImplBase[T, P] with SingleSelectionImplBase[T, P]
    with MultipleSelectionImplBase[T, P] {
  def filter(remote: Remote[P]): Boolean = remotes match {
    case Some(remotes) => remotes contains remote
    case None => true
  }
  def remote: Option[Remote[P]] = remotes collect {
    case remotes if remotes.size == 1 => remotes.head
  }
}

private[impl] object Selection {
  implicit def from[T, P <: Peer](selection: T from P)
    : Selection[T, P] = selection match {
      case selection: Selection[T, P] => selection
      case _ => throwRetierImplementationError(selection)
    }

  implicit def fromMultiple[T, P <: Peer](selection: T fromMultiple P)
    : Selection[T, P] = selection match {
      case selection: Selection[T, P] => selection
      case _ => throwRetierImplementationError(selection)
    }

  implicit def fromSingle[T, P <: Peer](selection: T fromSingle P)
    : Selection[T, P] = selection match {
      case selection: Selection[T, P] => selection
      case _ => throwRetierImplementationError(selection)
    }

  private def throwRetierImplementationError(ref: Any) =
    throw new RetierImplementationError(
      s"invalid selection implementation: ${className(ref)}")
}
