package loci
package network

import scala.concurrent.Future

trait ConnectionRequestor extends ConnectionEstablisher {
  def request: Future[Connection]
}
