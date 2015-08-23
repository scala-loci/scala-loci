package retier
package transmission

trait AbstractionId

trait AbstractionRef {
  def channel: Channel
  def derive(name: String): AbstractionRef
}
