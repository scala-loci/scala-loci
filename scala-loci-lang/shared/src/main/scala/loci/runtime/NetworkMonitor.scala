package loci.runtime

import loci.NetworkMonitorConfig
import loci.logging
import loci.utils.CollectionOps.ConcurrentLinkedQueueOps

import java.lang.{System => sys}
import java.util.TimerTask
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

class NetworkMonitor(config: NetworkMonitorConfig, remoteConnections: RemoteConnections) extends TimerTask {

  private val pingStore: ConcurrentLinkedQueue[Long] = new ConcurrentLinkedQueue[Long]

  override def run(): Unit = {
    remoteConnections.remotes.foreach { remote =>
      logging.info(s"Sending network monitoring message to $remote")
      remoteConnections.send(remote, NetworkMonitoringMessage(sys.currentTimeMillis(), isResponse = false))
    }
  }

  remoteConnections.receive.foreach {
    case (remote, NetworkMonitoringMessage(time, true)) =>
      val roundTripTime = sys.currentTimeMillis() - time
      pingStore.addAndLimit(roundTripTime, config.pingStorageCount)
      logging.info(s"Round-trip time for $remote: $roundTripTime")
      logging.info(pingStore.asScala.toSeq.toString)
    case _ =>
  }
}

class NetworkMonitorResponder(remoteConnections: RemoteConnections) {
  remoteConnections.receive.foreach {
    case (remote, NetworkMonitoringMessage(time, false)) =>
      logging.info(s"Returning network monitoring message to $remote")
      remoteConnections.send(remote, NetworkMonitoringMessage(time, isResponse = true))
    case _ =>
  }
}
