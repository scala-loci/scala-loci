package loci
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
  type AbstractionRef = transmitter.AbstractionRef

  private[impl] def create(abstraction: AbstractionId, link: String,
      remote: RemoteRef, system: System): AbstractionRef =
    AbstractionRefImpl(abstraction, link, remote, system)

  implicit class AbstractionRefOps(abstractionRef: AbstractionRef) {
    def abstraction: AbstractionId = abstractionRef match {
      case AbstractionRefImpl(abstraction, _, _, _) => abstraction
      case _ => throwLociImplementationError(abstractionRef)
    }

    def link: String = abstractionRef match {
      case AbstractionRefImpl(_, link, _, _) => link
      case _ => throwLociImplementationError(abstractionRef)
    }

    def remote: RemoteRef = abstractionRef match {
      case AbstractionRefImpl(_, _, remote, _) => remote
      case _ => throwLociImplementationError(abstractionRef)
    }

    private def throwLociImplementationError(ref: Any) =
      throw new LociImplementationError(
        s"invalid abstraction reference implementation: ${className(ref)}")
  }
}
