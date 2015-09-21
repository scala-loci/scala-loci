package object retier extends
    ImplicitTransmissions with
    ImplicitConversions with
    ide.intellij.ImplicitConversions {
  type LocalDeclaration = Any

  type RemoteMethod[+T, P <: Peer] = T on P

  type RemoteProperty[+T, P <: Peer] = T on P


  type on[+T, P <: Peer] = T sharedOn P


  def `abstract`[P <: Peer, T](implicit ev: NoLocalPeer[_]): T on P = `#macro`

  def placed[P <: Peer]:
    PlacingExpression[P] with OverridingExpression[P] = `#macro`

  def remote[P <: Peer]:
    RemoteExpression[P, on] with RemoteSelectionExpression[P] = `#macro`


  def peerTypeOf[P](implicit tag: PeerTypeTag[P]): PeerType = tag.peerType


  final implicit class FromExpression[L <: Peer, R <: Peer, T]
      (v: T sharedOn R)
      (implicit ev: LocalPeer[L]) {
    def from[P <: R]: T from P = `#macro`
    def from[P <: R](peer: Remote[P]): T fromSingle P = `#macro`
    def from[P <: R](peers: Remote[P]*): T fromMultiple P = `#macro`
  }

  def `#macro`: Nothing =
    throw new NotImplementedError("Only usable in `multitier` environment")
}
