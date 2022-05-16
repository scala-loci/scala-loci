package loci
package communicator
package ws

package object akka {
  private[akka] def unavailable = sys.error("Akka WebSocket communicator only available on the JVM")
}
