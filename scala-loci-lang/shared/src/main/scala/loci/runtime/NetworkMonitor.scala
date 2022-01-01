package loci.runtime

import loci.NetworkMonitorConfig
import loci.PingInfo
import loci.Remote
import loci.TransmittedBytesInfo
import loci.logging
import loci.utils.CollectionOps.ConcurrentHashMapOps
import loci.utils.CollectionOps.ConcurrentLinkedQueueOps

import java.lang.{System => sys}
import java.util.TimerTask
import java.util.concurrent.ConcurrentLinkedQueue
import loci.{NetworkMonitor => NetworkMonitorAPI}

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.CollectionHasAsScala

class NetworkMonitor(config: NetworkMonitorConfig, remoteConnections: RemoteConnections) extends TimerTask
  with NetworkMonitorAPI {

  private val pingStore: ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[PingInfo]] =
    new ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[PingInfo]]

  private val receivedBytes: ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[TransmittedBytesInfo]] =
    new ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[TransmittedBytesInfo]]

  private val sentBytes: ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[TransmittedBytesInfo]] =
    new ConcurrentHashMap[Remote[Nothing], ConcurrentLinkedQueue[TransmittedBytesInfo]]

  override def run(): Unit = {
    remoteConnections.remotes.foreach { remote =>
      logging.info(s"Sending network monitoring message to $remote")
      remoteConnections.send(remote, NetworkMonitoringMessage(sys.currentTimeMillis(), isResponse = false))
    }
  }

  remoteConnections.receive.foreach {
    case (remote, NetworkMonitoringMessage(time, true)) =>
      val roundTripTime = sys.currentTimeMillis() - time
      pingStore
        .getOrDefaultWithPut(remote, new ConcurrentLinkedQueue[PingInfo])
        .addAndLimit(
          PingInfo(roundTripTime, LocalDateTime.now()),
          LocalDateTime.now().minus(config.pingStorageDuration.toMillis, ChronoUnit.MILLIS)
        )
      logging.info(s"Round-trip time for $remote: $roundTripTime")
    case (remote, ChannelMessage(_, _, _, messageBuffer)) =>
      val bytes = messageBuffer.length
      receivedBytes
        .getOrDefaultWithPut(remote, new ConcurrentLinkedQueue[TransmittedBytesInfo])
        .addAndLimit(
            TransmittedBytesInfo(bytes, LocalDateTime.now()),
            LocalDateTime.now().minus(config.transmittedBytesStorageDuration.toMillis, ChronoUnit.MILLIS)
        )
      logging.info(s"Received $bytes bytes from $remote")
    case _ =>
  }

  remoteConnections.sendNotice.foreach {
    case (remote, ChannelMessage(_, _, _, messageBuffer)) =>
      val bytes = messageBuffer.length
      sentBytes
        .getOrDefaultWithPut(remote, new ConcurrentLinkedQueue[TransmittedBytesInfo])
        .offer(TransmittedBytesInfo(bytes, LocalDateTime.now()))
      logging.info(s"Sent $bytes bytes to $remote")
    case _ =>
  }

  override def getStoredPings[P](remote: Remote[P]): Seq[PingInfo] =
    pingStore.getOrDefault(remote, new ConcurrentLinkedQueue[PingInfo]).asScala.toSeq

  override def getSentBytes[P](remote: Remote[P]): Seq[TransmittedBytesInfo] =
    sentBytes.getOrDefault(remote, new ConcurrentLinkedQueue[TransmittedBytesInfo]).asScala.toSeq

  override def getReceivedBytes[P](remote: Remote[P]): Seq[TransmittedBytesInfo] =
    receivedBytes.getOrDefault(remote, new ConcurrentLinkedQueue[TransmittedBytesInfo]).asScala.toSeq
}

class NetworkMonitorResponder(remoteConnections: RemoteConnections) {
  remoteConnections.receive.foreach {
    case (remote, NetworkMonitoringMessage(time, false)) =>
      logging.info(s"Returning network monitoring message to $remote")
      remoteConnections.send(remote, NetworkMonitoringMessage(time, isResponse = true))
    case _ =>
  }
}
