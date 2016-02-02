package object retier extends
    ImplicitTransmissions with
    ImplicitConversions with
    ide.intellij.ImplicitConversions {
  type LocalDeclaration = Any

  type RemoteMethod[+T, P <: Peer] = T on P

  type RemoteProperty[+T, P <: Peer] = T on P


  type on[+T, P <: Peer] = T sharedOn P


  def placed[P <: Peer]:
    SpecialPlacingExpression[P] with PlacingExpression[P] with
    OverridingExpression[P] = `#macro`

  def remote[P <: Peer]:
    RemoteExpression[P, on] with RemoteSelectionExpression[P] with
    RemoteConnectionExpression[P] = `#macro`


  def peerTypeOf[P](implicit tag: PeerTypeTag[P]): PeerType = tag.peerType


  def `#macro`: Nothing =
    throw new NotImplementedError("only usable in `multitier` environment")
}
