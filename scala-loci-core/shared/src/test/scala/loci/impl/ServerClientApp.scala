package loci
package impl

import transmitter._
import scala.util.Try
import Marshallable.Int

object Marshallable {
  implicit object Int extends MarshallableArgument[Int] {
    def marshal(value: Int, abstraction: AbstractionRef) = MessageBuffer fromString value.toString
    def unmarshal(value: MessageBuffer, abstraction: AbstractionRef) = Try { (value toString (0, value.length)).toInt }
    def isPushBased = false
  }
}

@multitier
class ServerClientApp(listener: NetworkListener) {
  class Server extends Peer {
    type Tie = Multiple[Client]
    def connect = listen[Client] { listener }
    override def context = contexts.Immediate.global
  }

  class Client extends Peer {
    type Tie = Single[Server]
    def connect = connect[Server] { listener.createConnector }
    override def context = contexts.Immediate.global
  }

  val id: Int on Client = 0

  def square(a: Int) = placed[Server] { implicit! => a * a }
}
