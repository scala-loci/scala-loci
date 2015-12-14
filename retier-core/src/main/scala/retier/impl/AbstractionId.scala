package retier
package impl

import AbstractionId._

private final case class AbstractionIdImpl(name: String)(val isStable: Boolean)
    extends AbstractionId

object AbstractionId {
  type AbstractionId = transmission.AbstractionId

  def create(name: String, isStable: Boolean = false): AbstractionId =
    AbstractionIdImpl(name)(isStable)

  implicit class AbstractionIdOps(abstraction: AbstractionId) {
    def name: String = abstraction match {
      case abstraction @ AbstractionIdImpl(_) => abstraction.name
      case _ => throwRetierImplementationError(abstraction)
    }

    def isStable: Boolean = abstraction match {
      case abstraction @ AbstractionIdImpl(_) => abstraction.isStable
      case _ => throwRetierImplementationError(abstraction)
    }

    private def throwRetierImplementationError(ref: Any) =
      throw new RetierImplementationError(
        s"invalid abstraction id implementation: ${className(ref)}")
  }
}
