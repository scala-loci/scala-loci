package loci
package ide.intellij

import scala.language.implicitConversions

protected[loci] trait RemoteConnectionConversions {
  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$multipleConnectionDefault
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, MultipleTie],
        ev2: MultipleRemoteConnection[R] => MultipleRemoteConnection.DefaultMultipleRemoteConnection[R]):
    MultipleRemoteConnection.DefaultMultipleRemoteConnection[R] = ???

  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$optionalConnectionDefault
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, OptionalTie],
        ev2: OptionalRemoteConnection[R] => OptionalRemoteConnection.DefaultOptionalRemoteConnection[R]):
    OptionalRemoteConnection.DefaultOptionalRemoteConnection[R] = ???

  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$loci$intellij$singleConnectionDefault
    [R <: Peer, L <: Peer]
    (v: RemoteConnectionExpression[R])
    (implicit
        dummy: IntelliJDummy,
        ev0: LocalPeer[L],
        ev1: PeerTie[L#Tie, R, SingleTie],
        ev2: SingleRemoteConnection[R] => SingleRemoteConnection.DefaultSingleRemoteConnection[R]):
    SingleRemoteConnection.DefaultSingleRemoteConnection[R] = ???
}
