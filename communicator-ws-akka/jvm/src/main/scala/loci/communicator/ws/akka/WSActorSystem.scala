package loci
package communicator
package ws.akka

import contexts.Immediate.Implicits.global

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory

import scala.concurrent.Future

private object WSActorSystem {
  locally(WSActorSystem)

  private var count = 0
  private implicit var actorSystem: ActorSystem = _
  private implicit var actorMaterializer: ActorMaterializer = _

  private var whenTerminated = Future.successful(())
  private[akka] def terminated = synchronized { whenTerminated }

  def retrieve(): (ActorSystem, ActorMaterializer) = synchronized {
    if (count == 0) {
      val defaultConfig = ConfigFactory.load()
      val config = defaultConfig.getConfig("loci.communicator.ws.akka").withFallback(defaultConfig)

      actorSystem = ActorSystem("websocket-system", config)
      actorMaterializer = ActorMaterializer()
      whenTerminated = actorSystem.whenTerminated map Function.const(())
    }
    count += 1
    (actorSystem, actorMaterializer)
  }

  def release() = synchronized {
    if (count > 0)
      count -= 1

    if (count == 0) {
      actorMaterializer.shutdown()
      actorSystem.terminate()
      actorMaterializer = null
      actorSystem = null
    }
  }
}
