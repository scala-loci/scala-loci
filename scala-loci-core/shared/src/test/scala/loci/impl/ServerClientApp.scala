package loci
package impl

import scala.util.Try
import Marshallable.Int

object Marshallable {
  implicit object Int extends transmission.MarshallableArgument[Int] {
    def marshal(value: Int, abstraction: transmission.AbstractionRef) =
      value.toString
    def unmarshal(value: String, abstraction: transmission.AbstractionRef) =
      Try { value.toInt }
    val isPushBased = false
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
    def connect = request[Server] { listener.createRequestor }
    override def context = contexts.Immediate.global
  }

  val id: Int on Client = 0

  def square(a: Int) = placed[Server] { implicit! => a * a }
}
