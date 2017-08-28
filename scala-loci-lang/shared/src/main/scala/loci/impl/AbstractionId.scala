package loci
package impl

import AbstractionId._

private final case class AbstractionIdImpl(name: String)(val isStable: Boolean)
    extends AbstractionId

object AbstractionId {
  type AbstractionId = transmitter.AbstractionId

  def create(name: String, isStable: Boolean = false): AbstractionId =
    AbstractionIdImpl(name)(isStable)

  implicit class AbstractionIdOps(abstraction: AbstractionId) {
    def name: String = abstraction match {
      case abstraction @ AbstractionIdImpl(_) => abstraction.name
      case _ => throwLociImplementationError(abstraction)
    }

    def isStable: Boolean = abstraction match {
      case abstraction @ AbstractionIdImpl(_) => abstraction.isStable
      case _ => throwLociImplementationError(abstraction)
    }

    private def throwLociImplementationError(ref: Any) =
      throw new LociImplementationError(
        s"invalid abstraction id implementation: ${className(ref)}")
  }
}
