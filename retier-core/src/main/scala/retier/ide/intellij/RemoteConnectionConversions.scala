package retier
package ide.intellij

import scala.language.implicitConversions

protected[retier] trait RemoteConnectionConversions {
  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$multipleConnectionDefault
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, MultipleConnection],
        ev2: MultipleRemoteConnection[R] => MultipleRemoteConnection.DefaultMultipleRemoteConnection[R]):
    MultipleRemoteConnection.DefaultMultipleRemoteConnection[R] = ???

  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$optionalConnectionDefault
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, OptionalConnection],
        ev2: OptionalRemoteConnection[R] => OptionalRemoteConnection.DefaultOptionalRemoteConnection[R]):
    OptionalRemoteConnection.DefaultOptionalRemoteConnection[R] = ???

  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$singleConnectionDefault
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, SingleConnection],
        ev2: SingleRemoteConnection[R] => SingleRemoteConnection.DefaultSingleRemoteConnection[R]):
    SingleRemoteConnection.DefaultSingleRemoteConnection[R] = ???
}
