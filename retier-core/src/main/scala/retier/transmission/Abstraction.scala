package retier
package transmission

trait AbstractionId

trait AbstractionRef {
  def openChannel: Channel
  def derive(name: String): AbstractionRef
}
