package object retier extends
    ImplicitTransmissions with
    ImplicitConversions with ide.intellij.ImplicitConversions {
  type LocalDeclaration[+T, P <: Peer] = T on P

  type RemoteMethod[+T, P <: Peer] = T on P

  type RemoteProperty[+T, P <: Peer] = T on P


  type on[+T, P <: Peer] = T `shared on` P


  def `abstract`[P <: Peer, T](implicit ev: NoLocalPeer[_]): T on P = `#macro`

  def placed[P <: Peer]:
    PlacingExpression[P] with OverridingExpression[P] = `#macro`

  def remote[P <: Peer]:
    RemoteExpression[P, on] with RemoteSelectionExpression = `#macro`


  final implicit class FromExpression[L <: Peer, R <: Peer, T]
      (v: T `shared on` R)
      (implicit ev: LocalPeer[L]) {
    def from[P <: Peer]
      (implicit ev: P <:< R): T on P = `#macro`
    def from[P <: Peer](peer: Remote[P])
      (implicit ev: P <:< R): T `from single` P = `#macro`
    def from[P <: Peer](peers: Remote[P]*)
      (implicit ev: P <:< R): T `from multiple` P = `#macro`
  }

  def `#macro`: Nothing =
    throw new NotImplementedError("Only usable in multitier environment")
}
