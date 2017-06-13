package loci
package communicator
package ws.akka

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory

private object WSActorSystem {
  private var count = 0
  @volatile private implicit var actorSystem: ActorSystem = _
  @volatile private implicit var actorMaterializer: ActorMaterializer = _

  def retrieve(): (ActorSystem, ActorMaterializer) = synchronized {
    if (count == 0) {
      val config = ConfigFactory parseString
        """
        akka.http {
          server { remote-address-header = on }
          parsing { tls-session-info-header = on }
        }
        """

      actorSystem = ActorSystem("websocket-system", config)
      actorMaterializer = ActorMaterializer()
    }
    count += 1
    (actorSystem, actorMaterializer)
  }

  def release() = synchronized {
    count -= 1
    if (count == 0) {
      actorMaterializer.shutdown
      actorSystem.terminate
      actorMaterializer = null
      actorSystem = null
    }
  }
}
