package retier
package network

import scala.concurrent.Future

trait ConnectionRequestor extends ConnectionEstablisher {
  def request: Future[Connection]
}
