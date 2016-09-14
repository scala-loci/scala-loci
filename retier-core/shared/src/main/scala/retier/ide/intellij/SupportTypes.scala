package retier
package ide.intellij

protected[retier] trait LocalValueTypes extends LocalValueTypesFastMacro {
  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$localValueTypes[T, U, Dummy]
    (implicit
        ev0: IntelliJDummy,
        ev1: ValueTypes[T, _, Dummy, U]): retier.LocalValueTypes[T, U] = ???
}

protected[retier] trait RemoteValueTypes extends RemoteValueTypesFastMacro {
  @annotation.compileTimeOnly(
    "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
  implicit def $$retier$intellij$remoteValueTypes[T, R <: Remote[Peer], U, Dummy]
    (implicit
        ev0: IntelliJDummy,
        ev1: ValueTypes[T, R, U, Dummy]): retier.RemoteValueTypes[T, R, U] = ???
}
