package loci

import scala.concurrent.duration.Duration

case class NetworkMonitorConfig(
  pingPeriod: Duration,
  pingStorageCount: Int
)
