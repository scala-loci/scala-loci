package retier

import network.ConnectionFactory
import network.ConnectionListener
import network.ConnectionRequestor
import util.Attributes
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.implicitConversions

trait Peer {
  // `Single` and `Optional` are invariant in `P`, while `Multiple` is covariant.
  // This is because type inference may infer a super-type `S` of `P` and it is
  // possible that other sub-types of S are part of the connection spec compound.
  // Therefore, when inferring a super-type of `P`, `Multiple` must be inferred.
  sealed trait ConnectionSpec
  sealed trait Single[P] extends Multiple[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P] extends ConnectionSpec


  type Connection <: ConnectionSpec

  def connect: ConnectionSetup[ConnectionSetupSpec]

  def context: ExecutionContext = implicitly[ExecutionContext]

  def setup: Boolean = true


  sealed trait ConnectionSetupSpec
  sealed trait Request[+P] extends ConnectionSetupSpec
  sealed trait Listen[+P] extends ConnectionSetupSpec

  sealed trait ConnectionSetup[+T] {
    def listeners(peerType: PeerType): List[ConnectionListener]
    def requestors(peerType: PeerType): List[ConnectionRequestor]
  }

  protected[this] object ConnectionSetup {
    final implicit def listenerToSetup
        (listener: ConnectionListener): ConnectionSetup[Listen[Nothing]] =
      new ConnectionSetup[Listen[Nothing]] {
        def listeners(peerType: PeerType) = List(listener)
        def requestors(peerType: PeerType) = List.empty
      }

    final implicit def requestorToSetup
        (requestor: ConnectionRequestor): ConnectionSetup[Request[Nothing]] =
      new ConnectionSetup[Request[Nothing]] {
        def listeners(peerType: PeerType) = List.empty
        def requestors(peerType: PeerType) = List(requestor)
      }

    implicit class Composition[T](self: ConnectionSetup[T]) {
      def and[U](setup: ConnectionSetup[U]): ConnectionSetup[T with U] =
        new ConnectionSetup[T with U] {
          def listeners(peerType: PeerType) =
            (self listeners peerType) ++ (setup listeners peerType)
          def requestors(peerType: PeerType) =
            (self requestors peerType) ++ (setup requestors peerType)
        }
    }
  }

  protected[this] final def listen[P: PeerTypeTag]
      (setup: ConnectionSetup[Listen[P]]): ConnectionSetup[Listen[P]] =
    new ConnectionSetup[Listen[P]] {
      def listeners(peerType: PeerType) =
        if (peerType == peerTypeOf[P]) setup listeners peerType else List.empty
      def requestors(peerType: PeerType) = List.empty
    }

  protected[this] final def request[P: PeerTypeTag]
      (setup: ConnectionSetup[Request[P]]): ConnectionSetup[Request[P]] =
    new ConnectionSetup[Request[P]] {
      def listeners(peerType: PeerType) = List.empty
      def requestors(peerType: PeerType) =
        if (peerType == peerTypeOf[P]) setup requestors peerType else List.empty
    }

  protected[this] final def listen[P: PeerTypeTag]
      (factory: ConnectionFactory)
      (config: String,
       attrs: Attributes = Attributes.empty): ConnectionSetup[Listen[P]] =
    new ConnectionSetup[Listen[P]] {
      def listeners(peerType: PeerType) =
        (factory listener (config, attrs)).toList
      def requestors(peerType: PeerType) = List.empty
    }

  protected[this] final def request[P: PeerTypeTag]
      (factory: ConnectionFactory)
      (url: String,
       attrs: Attributes = Attributes.empty): ConnectionSetup[Request[P]] =
    new ConnectionSetup[Request[P]] {
      def listeners(peerType: PeerType) = List.empty
      def requestors(peerType: PeerType) =
        (factory requestor (url, attrs)).toList
    }

  protected[this] final def load[T]
      (factory: ConnectionFactory)
      (configfile: String)
      (implicit
        parser: ConfigurationParser,
        peerTypeTag: PeerTypeTag[this.type]): ConnectionSetup[T] = {
    val config = parser load (configfile, peerTypeTag.peerType)

    val peerListeners =
      config.listeners map { case (peerType, config, attrs) =>
        peerType -> (factory listener (config, attrs))
      } collect { case (peerType, Some(listener)) => peerType -> listener }

    val peerRequestors =
      config.requestors map { case (peerType, url, attrs) =>
        peerType -> (factory requestor (url, attrs))
      } collect { case (peerType, Some(requestor)) => peerType -> requestor }

    new ConnectionSetup[T] {
      def listeners(peerType: PeerType): List[ConnectionListener] =
        peerListeners collect { case (`peerType`, listener) => listener }
      def requestors(peerType: PeerType): List[ConnectionRequestor] =
        peerRequestors collect { case (`peerType`, requestor) => requestor }
    }
  }
}
