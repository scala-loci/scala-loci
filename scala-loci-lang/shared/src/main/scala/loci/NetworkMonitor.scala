package loci

trait NetworkMonitor {
  def getStoredPings[P](remote: Remote[P]): Seq[Long]
}
