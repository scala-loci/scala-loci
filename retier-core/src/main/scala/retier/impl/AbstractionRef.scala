package retier
package impl

import AbstractionId._
import AbstractionRef._
import RemoteRef._

private final case class AbstractionRefImpl(abstraction: AbstractionId,
    link: String, remote: RemoteRef, system: System) extends AbstractionRef {
  def channel = system.obtainChannel(link, remote)
  def derive(name: String) = AbstractionRef.create(
    AbstractionId.create(abstraction.name, false),
    link + ":" + name, remote, system)
}

object AbstractionRef {
  type AbstractionRef = transmission.AbstractionRef

  private[impl] def create(abstraction: AbstractionId, link: String,
      remote: RemoteRef, system: System): AbstractionRef =
    AbstractionRefImpl(abstraction, link, remote, system)

  implicit class AbstractionRefOps(abstractionRef: AbstractionRef) {
    def abstraction: AbstractionId = abstractionRef match {
      case AbstractionRefImpl(abstraction, _, _, _) => abstraction
      case _ => throwRetierImplementationError(abstractionRef)
    }

    def link: String = abstractionRef match {
      case AbstractionRefImpl(_, link, _, _) => link
      case _ => throwRetierImplementationError(abstractionRef)
    }

    def remote: RemoteRef = abstractionRef match {
      case AbstractionRefImpl(_, _, remote, _) => remote
      case _ => throwRetierImplementationError(abstractionRef)
    }

    private def throwRetierImplementationError(ref: Any) =
      throw new RetierImplementationError(
        s"invalid abstraction reference implementation: ${className(ref)}")
  }
}
