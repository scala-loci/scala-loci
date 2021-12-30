package loci.runtime

import loci.NetworkMonitorConfig
import loci.Remote
import loci.logging
import loci.utils.CollectionOps.ConcurrentHashMapOps
import loci.utils.CollectionOps.ConcurrentLinkedQueueOps

import java.lang.{System => sys}
import java.util.TimerTask
import java.util.concurrent.ConcurrentLinkedQueue
import loci.{NetworkMonitor => NetworkMonitorAPI}

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

class NetworkMonitor(config: NetworkMonitorConfig, remoteConnections: RemoteConnections) extends TimerTask
  with NetworkMonitorAPI {

  private val pingStore: ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[Long]] =
    new ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[Long]]

  override def run(): Unit = {
    remoteConnections.remotes.foreach { remote =>
      logging.info(s"Sending network monitoring message to $remote")
      remoteConnections.send(remote, NetworkMonitoringMessage(sys.currentTimeMillis(), isResponse = false))
    }
  }

  remoteConnections.receive.foreach {
    case (remote, NetworkMonitoringMessage(time, true)) =>
      val roundTripTime = sys.currentTimeMillis() - time
      pingStore.getOrDefaultWithPut(remote, new ConcurrentLinkedQueue[Long]).addAndLimit(roundTripTime, config.pingStorageCount)
      logging.info(s"Round-trip time for $remote: $roundTripTime")
    case _ =>
  }

  override def getStoredPings[P](remote: Remote[P]): Seq[Long] =
    pingStore.getOrDefault(remote, new ConcurrentLinkedQueue[Long]).asScala.toSeq
}

class NetworkMonitorResponder(remoteConnections: RemoteConnections) {
  remoteConnections.receive.foreach {
    case (remote, NetworkMonitoringMessage(time, false)) =>
      logging.info(s"Returning network monitoring message to $remote")
      remoteConnections.send(remote, NetworkMonitoringMessage(time, isResponse = true))
    case _ =>
  }
}
