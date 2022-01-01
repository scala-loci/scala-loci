package loci

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.Duration

trait NetworkMonitor {

  def getStoredPings[P](remote: Remote[P]): Seq[PingInfo]

  def getSentBytes[P](remote: Remote[P]): Seq[TransmittedBytesInfo]

  def getReceivedBytes[P](remote: Remote[P]): Seq[TransmittedBytesInfo]

  def sumSentBytes[P](remote: Remote[P], last: Duration = Duration.Inf): Long = sumBytes(getSentBytes(remote), last)

  def sumReceivedBytes[P](remote: Remote[P], last: Duration = Duration.Inf): Long = sumBytes(getReceivedBytes(remote), last)

  private def sumBytes[P](bytes: Seq[TransmittedBytesInfo], last: Duration): Long = {
    val bytesInTimeRange = last match {
      case Duration.Inf => bytes
      case last =>
        bytes.dropWhile(_.timestamp.isBefore(LocalDateTime.now().minus(last.toMillis, ChronoUnit.MILLIS)))
    }
    bytesInTimeRange.map(_.bytes.toLong).sum
  }

  def averagePing[P](remote: Remote[P], last: Duration = Duration.Inf): Double = {
    val pingsInTimeRange = last match {
      case Duration.Inf => getStoredPings(remote)
      case last =>
        getStoredPings(remote).dropWhile(_.timestamp.isBefore(LocalDateTime.now().minus(last.toMillis, ChronoUnit.MILLIS)))
    }
    pingsInTimeRange.map(_.ping).sum.toDouble / pingsInTimeRange.size
  }

}

case class TransmittedBytesInfo(
  bytes: Int,
  timestamp: LocalDateTime
)

case class PingInfo(
  ping: Long,
  timestamp: LocalDateTime
)
