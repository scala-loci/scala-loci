package loci
package communicator
package ws

package object javalin {
  private[javalin] def unavailable = sys.error("Javalin WebSocket communicator only available on the JVM")
}
