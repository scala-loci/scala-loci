package loci
package ide.intellij

import rescala.graph.Struct
import rescala.engines.Engine
import rescala.propagation.Turn
import scala.language.implicitConversions

protected[loci] trait ReactiveRemoteConnectionInterface {
    this: rescalaTransmitter.type =>

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$multipleConnectionRescala
    [R <: Peer, L <: Peer, S <: Struct]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, MultipleTie],
        ev2: Engine[S, Turn[S]]):
    RescalaMultipleRemoteConnectionInterface[R, S] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$optionalConnectionRescala
    [R <: Peer, L <: Peer, S <: Struct]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, OptionalTie],
        ev2: Engine[S, Turn[S]]):
    RescalaOptionalRemoteConnectionInterface[R, S] = ???

  @annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$singleConnectionRescala
    [R <: Peer, L <: Peer, S <: Struct]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, SingleTie],
        ev2: Engine[S, Turn[S]]):
    RescalaSingleRemoteConnectionInterface[R, S] = ???
}
