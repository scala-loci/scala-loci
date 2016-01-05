package retier
package ide.intellij

import scala.language.implicitConversions

protected[retier] trait ReactiveRemoteConnectionInterface {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$multipleConnectionRescala
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, MultipleConnection],
        ev2: MultipleRemoteConnection[R] => RescalaMultipleRemoteConnectionInterface[R]):
    RescalaMultipleRemoteConnectionInterface[R] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$optionalConnectionRescala
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, OptionalConnection],
        ev2: OptionalRemoteConnection[R] => RescalaOptionalRemoteConnectionInterface[R]):
    RescalaOptionalRemoteConnectionInterface[R] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$singleConnectionRescala
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, SingleConnection],
        ev2: SingleRemoteConnection[R] => RescalaSingleRemoteConnectionInterface[R]):
    RescalaSingleRemoteConnectionInterface[R] = ???
}
