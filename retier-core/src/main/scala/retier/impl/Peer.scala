package retier
package impl

import AbstractionId._
import AbstractionRef._
import scala.util.Try
import scala.util.Failure

trait PeerImpl {
  def $$retier$system: System

  def $$retier$dispatch(request: String, id: AbstractionId, ref: AbstractionRef)
    : Try[String] = Failure(
        new RetierImplementationError(
          s"request for ${id.name} could not be dispatched"))

  def $$retier$main(): Unit = { }
  def $$retier$terminating(): Unit = { }
  def $$retier$error(): Unit = { }
  def $$retier$fatal(): Unit = { }
}

object PeerImpl {
  val peerTypeTag = PeerTypeTag.create[Peer]("Peer", List.empty)

  implicit class Ops(peerImpl: PeerImpl) {
    def dispatch(request: String, id: AbstractionId, ref: AbstractionRef) =
      peerImpl.$$retier$dispatch(request, id, ref)
    def main() =
      peerImpl.$$retier$main
    def terminating() =
      peerImpl.$$retier$terminating
    def error() =
      peerImpl.$$retier$error
    def fatal() =
      peerImpl.$$retier$fatal
  }
}
