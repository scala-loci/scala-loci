package loci

import transmitter.RemoteRef

trait Remote[+P <: Peer] extends RemoteRef {
  def authenticated: Boolean
  def authenticate(): Unit
}
