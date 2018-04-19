package loci
package impl

import AbstractionId._
import AbstractionRef._
import scala.util.Try
import scala.util.Failure

trait PeerImpl {
  def $$loci$system: System = PeerImpl.throwSystemNotSetUp

  def $$loci$metapeer: Peer = PeerImpl.throwMetaPeerNotSetUp

  def $$loci$dispatch(request: String, id: AbstractionId, ref: AbstractionRef)
    : Try[String] = Failure(
        new LociImplementationError(
          s"request for ${id.name} could not be dispatched"))

  def $$loci$main(): Unit = { }
  def $$loci$terminating(): Unit = { }
  def $$loci$error(): Unit = { }
  def $$loci$fatal(): Unit = { }
}

object PeerImpl {
  val peerTypeTag = PeerTypeTag.create[Peer]("Peer", List.empty)

  def throwSystemNotSetUp =
    throw new LociImplementationError("peer instance system not set up")

  def throwMetaPeerNotSetUp =
    throw new LociImplementationError("peer meta object not set up")

  implicit class Ops(peerImpl: PeerImpl) {
    def dispatch(request: String, id: AbstractionId, ref: AbstractionRef) =
      peerImpl.$$loci$dispatch(request, id, ref)
    def main() =
      peerImpl.$$loci$main
    def terminating() =
      peerImpl.$$loci$terminating
    def error() =
      peerImpl.$$loci$error
    def fatal() =
      peerImpl.$$loci$fatal
  }
}
