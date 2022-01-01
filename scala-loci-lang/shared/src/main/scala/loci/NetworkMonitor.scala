package loci

import java.time.LocalDateTime

trait NetworkMonitor {
  def getStoredPings[P](remote: Remote[P]): Seq[PingInfo]
  def getSentBytes[P](remote: Remote[P]): Seq[TransmittedBytesInfo]
  def getReceivedBytes[P](remote: Remote[P]): Seq[TransmittedBytesInfo]
}

case class TransmittedBytesInfo(
  bytes: Int,
  timestamp: LocalDateTime
)

case class PingInfo(
  ping: Long,
  timestamp: LocalDateTime
)
