package loci

import communicator.ProtocolCommon
import communicator.Bidirectional
import communicator.ConnectionSetupFactory
import communicator.Listener
import communicator.Connector
import messaging.ConnectionsBase.Protocol
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

trait Peer {
  // `Single` and `Optional` are invariant in `P`, while `Multiple` is covariant.
  // This is because type inference may infer a super-type `S` of `P` and it is
  // possible that other sub-types of S are part of the tie spec compound.
  // Therefore, when inferring a super-type of `P`, `Multiple` must be inferred.
  sealed trait TieSpec
  sealed trait Single[P] extends Optional[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P] extends TieSpec


  type Tie <: TieSpec

  def connect: ConnectionSetup

  def context: ExecutionContext = contexts.Queued.create


  sealed trait ConnectionSetup {
    def setup(peerType: PeerType, peerTypes: List[PeerType]):
      Map[PeerType, (List[Listener[Protocol]], List[Connector[Protocol]])]
  }

  protected[this] object Default {
    sealed trait Default[+D] extends Any
    sealed trait Connect[+P] extends Any
    sealed trait Listen[+P] extends Any

    def Listen[P]: Default[Listen[P]] = new Default[Listen[P]] { }
    def Connect[P]: Default[Connect[P]] = new Default[Connect[P]] { }
  }

  protected[this] object ConnectionSetup {
    final implicit def listenerToSetup[D, P](listener: Listener[Protocol])
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Listen[P],
          ev2: PeerTypeTag[P]): ConnectionSetup =
      new ConnectionSetup {
        def setup(peerType: PeerType, peerTypes: List[PeerType]) =
          Map(peerTypeOf[P] -> ((List(listener), List.empty)))
      }

    final implicit def connectorToSetup[D, P](connector: Connector[Protocol])
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Connect[P],
          ev2: PeerTypeTag[P]): ConnectionSetup =
      new ConnectionSetup {
        def setup(peerType: PeerType, peerTypes: List[PeerType]) =
          Map(peerTypeOf[P] -> ((List.empty, List(connector))))
      }

    implicit class Composition(self: ConnectionSetup) {
      def and(other: ConnectionSetup): ConnectionSetup =
        new ConnectionSetup {
          def setup(peerType: PeerType, peerTypes: List[PeerType]) = {
            val selfMap = self setup (peerType, peerTypes)
            val setupMap = other setup (peerType, peerTypes)
            ((selfMap.keySet ++ setupMap.keySet) map { key =>
              val (selfListeners, selfConnectors) =
                selfMap getOrElse (key, (List.empty, List.empty))
              val (setupListeners, setupConnectors) =
                setupMap getOrElse (key, (List.empty, List.empty))
              key -> ((
                selfListeners ++ setupListeners,
                selfConnectors ++ setupConnectors))
            }).toMap
          }
        }
    }
  }

  protected[this] trait SetupFactory[P <: ProtocolCommon] { thisFactory =>
    def listener(url: String, props: ConnectionSetupFactory.Properties):
      Option[Listener[P]]

    def connector(url: String, props: ConnectionSetupFactory.Properties):
      Option[Connector[P]]

    def and[O >: P <: ProtocolCommon](otherFactory: ConnectionSetupFactory[O]) =
      new SetupFactory[O] {
        override def listener(
            url: String, props: ConnectionSetupFactory.Properties) =
          (thisFactory listener (url, props)) orElse
          (otherFactory listener (url, props))

        override def connector(
            url: String, props: ConnectionSetupFactory.Properties) =
          (thisFactory connector (url, props)) orElse
          (otherFactory connector (url, props))
      }
  }

  protected[this] implicit def SetupFactory[P <: ProtocolCommon](
      factory: ConnectionSetupFactory[P]) =
    new SetupFactory[P] {
      def listener(url: String, props: ConnectionSetupFactory.Properties) =
        factory listener (url, props)

      def connector(url: String, props: ConnectionSetupFactory.Properties) =
        factory connector (url, props)
    }


  protected[this] final def manually: ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) = Map.empty
    }


  protected[this] case class FactorySetup(factory: SetupFactory[Protocol])

  protected[this] def setup(factory: SetupFactory[Protocol]) =
    FactorySetup(factory)

  protected[this] implicit class FactorySetupListener[D, P]
      (factorySetup: FactorySetup)
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Listen[P],
          ev2: PeerTypeTag[P]) {
    def apply(config: String) =
      listen[P] (factorySetup.factory) (config, Map.empty)
    def apply(config: String, props: ConnectionSetupFactory.Properties) =
      listen[P] (factorySetup.factory) (config, props)
  }

  protected[this] implicit class FactorySetupConnector[D, P]
      (factorySetup: FactorySetup)
      (implicit
          ev0: Default.Default[D],
          ev1: D <:< Default.Connect[P],
          ev2: PeerTypeTag[P]) {
    def apply(url: String) =
      connect[P] (factorySetup.factory) (url, Map.empty)
    def apply(url: String, props: ConnectionSetupFactory.Properties) =
      connect[P] (factorySetup.factory) (url, props)
  }


  protected[this] final def listen[P: PeerTypeTag]
      (listener: Listener[Protocol]): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] -> ((List(listener), List.empty)))
    }

  protected[this] final def connect[P: PeerTypeTag]
      (connector: Connector[Protocol]): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] -> ((List.empty, List(connector))))
    }

  protected[this] final def listen[P: PeerTypeTag]
      (factory: SetupFactory[Protocol])
      (config: String,
       props: ConnectionSetupFactory.Properties = Map.empty): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] ->
          (((factory listener (config, props)).toList, List.empty)))
    }

  protected[this] final def connect[P: PeerTypeTag]
      (factory: SetupFactory[Protocol])
      (url: String,
       props: ConnectionSetupFactory.Properties = Map.empty): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) =
        Map(peerTypeOf[P] ->
          ((List.empty, (factory connector (url, props)).toList)))
    }

  protected[this] final def load
      (factory: SetupFactory[Protocol])
      (configfile: String)
      (implicit parser: ConfigurationParser): ConnectionSetup =
    new ConnectionSetup {
      def setup(peerType: PeerType, peerTypes: List[PeerType]) = {
        val config = parser load (configfile, peerType, peerTypes)

        val peerListeners =
          (config.listeners map { case (peerType, config, props) =>
            peerType -> (factory listener (config, props))
          } groupBy { case (peerType, _) =>
            peerType
          } collect { case (peerType, listeners) =>
            peerType ->
              (listeners collect { case (_, Some(listener)) => listener })
          }).toMap

        val peerConnectors =
          (config.connectors map { case (peerType, url, props) =>
            peerType -> (factory connector (url, props))
          } groupBy { case (peerType, _) =>
            peerType
          } collect { case (peerType, connectors) =>
            peerType ->
              (connectors collect { case (_, Some(connector)) => connector })
          }).toMap

        ((peerListeners.keySet ++ peerConnectors.keySet) map { key =>
          val listeners = peerListeners getOrElse (key, List.empty)
          val connectors = peerConnectors getOrElse (key, List.empty)
          key -> ((listeners, connectors))
        }).toMap
      }
    }
}

object Peer {
  implicit val peerTypeTag = loci.impl.PeerImpl.peerTypeTag
}
