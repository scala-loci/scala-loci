import sbt._
import Keys._

object SourceGenerator {
  val usingExpressions = Seq(
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val exprs = (1 until 22) map { i =>
        val typeArgs = (0 to i) map { i => s"V$i, T$i" } mkString ", "
        val args = (0 to i) map { i => s"v$i: V$i" } mkString ", "
        val params = (0 to i) map { i => s"T$i" } mkString ", "
        val implicitArgs = (0 to i) map { i => s"""
          ev$i: ValueTypes[V$i, T$i]""" } mkString ", "

        val usingMember = s"""def using[$typeArgs]
         ($args)
         (implicit $implicitArgs): RemoteUsing${i+1}Expression[P, placed, $params]
        """

        val issuedUsingMember = s"""def using[$typeArgs]
         ($args)
         (implicit $implicitArgs): RemoteIssuedUsing${i+1}Expression[P, placed, $params]
        """

        val usingType = s"""
          protected final abstract class RemoteUsing${i+1}Expression[
              P <: Peer, placed[_, _ <: Peer], $params] {
            def in[T, U, L <: Peer]
             (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` ((
               $params) => T))
             (implicit
                 ev0: LocalPeer[L],
                 ev1: PlacingTypes[P, T, U],
                 ev2: PeerConnection[L#Connection, P, _]): U placed P
          }
        """

        val issuedUsingType = s"""
          protected final abstract class RemoteIssuedUsing${i+1}Expression[
              P <: Peer, placed[_, _ <: Peer], $params] {
            def in[T, U, I, L <: Peer]
             (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` ((
               $params) => T))
             (implicit
                 ev0: LocalPeer[L],
                 ev1: PlacingTypes[P, T, I],
                 ev2: IssuingTypes[L, I, U],
                 ev3: PeerConnection[L#Connection, P, _],
                 ev4: PeerConnection[P#Connection, L, _]): U placed P
          }
        """

        ((usingMember, issuedUsingMember), (usingType, issuedUsingType))
      }

      val (members, types) = exprs.unzip

      val (usingMembers, issuedUsingMembers) = members.unzip
      val (usingTypes, issuedUsingTypes) = types.unzip

      val files = Map(
        dir / "retier" / "RemoteExpressionUsing.scala" ->
        s"""package retier

          import dslparadise._
          import scala.language.higherKinds

          protected trait RemoteExpressionUsing[P <: Peer, placed[_, _ <: Peer]] {
            ${usingMembers.mkString}
          }

          ${usingTypes.mkString}
        """,

        dir / "retier" / "RemoteExpressionIssuedUsing.scala" ->
        s"""package retier

          import dslparadise._
          import scala.language.higherKinds

          protected trait RemoteExpressionIssuedUsing[P <: Peer, placed[_, _ <: Peer]] {
            ${issuedUsingMembers.mkString}
          }

          ${issuedUsingTypes.mkString}
        """
      )

      files foreach { case (file, content) => IO write (file, content) }
      files.keys.toSeq
    }
  )
}
