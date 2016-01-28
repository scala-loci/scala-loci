package retier
package ide.intellij

import rescala.turns.Engine
import rescala.turns.Turn
import rescala.graph.Spores
import scala.language.implicitConversions

protected[retier] trait ReactiveRemoteConnectionInterface {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$multipleConnectionRescala
    [R <: Peer, L <: Peer, S <: Spores]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, MultipleConnection],
        ev2: Engine[S, Turn[S]]):
    RescalaMultipleRemoteConnectionInterface[R, S] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$optionalConnectionRescala
    [R <: Peer, L <: Peer, S <: Spores]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, OptionalConnection],
        ev2: Engine[S, Turn[S]]):
    RescalaOptionalRemoteConnectionInterface[R, S] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$singleConnectionRescala
    [R <: Peer, L <: Peer, S <: Spores]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerConnection[L#Connection, R, SingleConnection],
        ev2: Engine[S, Turn[S]]):
    RescalaSingleRemoteConnectionInterface[R, S] = ???
}
