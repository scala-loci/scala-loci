package retier
package impl

import transmission.Channel
import transmission.RemoteRef

class System {
  def terminate(): Unit = ???
  def obtainChannel(name: String, remote: RemoteRef): Channel = ???
}
