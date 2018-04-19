package loci

import network.ConnectionFactory
import network.ConnectionListener
import network.ConnectionRequestor
import util.Attributes
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

trait Peer {
  // `Single` and `Optional` are invariant in `P`, while `Multiple` is covariant.
  // This is because type inference may infer a super-type `S` of `P` and it is
  // possible that other sub-types of S are part of the connection spec compound.
  // Therefore, when inferring a super-type of `P`, `Multiple` must be inferred.
  sealed trait ConnectionSpec
  sealed trait Single[P] extends Optional[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P] extends ConnectionSpec


  type Connection <: ConnectionSpec

  def connect: ConnectionSetup

  def context: ExecutionContext = contexts.Queued.create


  sealed trait ConnectionSetup {
    def setup(peerType: PeerType, peerTypes: List[PeerType]):
      Map[PeerType, (List[ConnectionListener], List[ConnectionRequestor])]
  }

  protected[this] object Default {
    sealed trait Default[+D] extends Any
    sealed trait Request[+P] extends Any
    sealed trait Listen[+P] extends Any

    def Listen[P]: Default[Listen[P]] = new Default[Listen[P]] { }
    def Request[P]: Default[Request[P]] = new Default[Request[P]] { }
  }

  protected[this] object ConnectionSetup {
    final implicit def listenerToSetup[D, P](listener: ConnectionListener)
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Listen[P],
          ev2: PeerTypeTag[P]): ConnectionSetup =
      new ConnectionSetup {
        def setup(peerType: PeerType, peerTypes: List[PeerType]) =
          Map(peerTypeOf[P] -> ((List(listener), List.empty)))
      }

    final implicit def requestorToSetup[D, P](requestor: ConnectionRequestor)
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Request[P],
          ev2: PeerTypeTag[P]): ConnectionSetup =
      new ConnectionSetup {
        def setup(peerType: PeerType, peerTypes: List[PeerType]) =
          Map(peerTypeOf[P] -> ((List.empty, List(requestor))))
      }

    implicit class Composition(self: ConnectionSetup) {
      def and(other: ConnectionSetup): ConnectionSetup =
        new ConnectionSetup {
          def setup(peerType: PeerType, peerTypes: List[PeerType]) = {
            val selfMap = self setup (peerType, peerTypes)
            val setupMap = other setup (peerType, peerTypes)
            ((selfMap.keySet ++ setupMap.keySet) map { key =>
              val (selfListeners, selfRequestors) =
                selfMap getOrElse (key, (List.empty, List.empty))
              val (setupListeners, setupRequestors) =
                setupMap getOrElse (key, (List.empty, List.empty))
              key -> ((
                selfListeners ++ setupListeners,
                selfRequestors ++ setupRequestors))
            }).toMap
          }
        }
    }
  }

  protected[this] final def manually: ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) = Map.empty
    }

  protected[this] case class FactorySetup(factory: ConnectionFactory)

  protected[this] def setup(factory: ConnectionFactory) = FactorySetup(factory)

  protected[this] implicit class FactorySetupListener[D, P]
      (factorySetup: FactorySetup)
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Listen[P],
          ev2: PeerTypeTag[P]) {
    def apply(config: String) =
      listen[P] (factorySetup.factory) (config, Attributes.empty)
    def apply(config: String, attrs: Attributes) =
      listen[P] (factorySetup.factory) (config, attrs)
  }

  protected[this] implicit class FactorySetupRequestor[D, P]
      (factorySetup: FactorySetup)
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Request[P],
          ev2: PeerTypeTag[P]) {
    def apply(url: String) =
      request[P] (factorySetup.factory) (url, Attributes.empty)
    def apply(url: String, attrs: Attributes) =
      request[P] (factorySetup.factory) (url, attrs)
  }

  protected[this] final def listen[P: PeerTypeTag]
      (listener: ConnectionListener): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] -> ((List(listener), List.empty)))
    }

  protected[this] final def request[P: PeerTypeTag]
      (requestor: ConnectionRequestor): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] -> ((List.empty, List(requestor))))
    }

  protected[this] final def listen[P: PeerTypeTag]
      (factory: ConnectionFactory)
      (config: String,
       attrs: Attributes = Attributes.empty): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] ->
          (((factory listener (config, attrs)).toList, List.empty)))
    }

  protected[this] final def request[P: PeerTypeTag]
      (factory: ConnectionFactory)
      (url: String,
       attrs: Attributes = Attributes.empty): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] ->
          ((List.empty, (factory requestor (url, attrs)).toList)))
    }

  protected[this] final def load
      (factory: ConnectionFactory)
      (configfile: String)
      (implicit parser: ConfigurationParser): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) = {
        val config = parser load (configfile, peerType, peerTypes)

        val peerListeners =
          (config.listeners map { case (peerType, config, attrs) =>
            peerType -> (factory listener (config, attrs))
          } groupBy { case (peerType, _) =>
            peerType
          } collect { case (peerType, listeners) =>
            peerType ->
              (listeners collect { case (_, Some(listener)) => listener })
          }).toMap

        val peerRequestors =
          (config.requestors map { case (peerType, url, attrs) =>
            peerType -> (factory requestor (url, attrs))
          } groupBy { case (peerType, _) =>
            peerType
          } collect { case (peerType, requestors) =>
            peerType ->
              (requestors collect { case (_, Some(requestor)) => requestor })
          }).toMap

        ((peerListeners.keySet ++ peerRequestors.keySet) map { key =>
          val listeners = peerListeners getOrElse (key, List.empty)
          val requestors = peerRequestors getOrElse (key, List.empty)
          key -> ((listeners, requestors))
        }).toMap
      }
    }
}

object Peer {
  implicit val peerTypeTag = loci.impl.PeerImpl.peerTypeTag
}
