import sbt._
import Keys._

object SourceGenerator {
  val usingExpressions = Seq(
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val exprs = (1 until 22) map { i =>
        val typeArgs = (0 to i) map { i => s"V$i" } mkString ", "
        val args = (0 to i) map { i => s"v$i: V$i" } mkString ", "
        val capturedArgs = (0 to i) map { i => s"Captured[V$i]" } mkString ", "

        val usingExpr = s"""def using[T, U, L <: Peer, $typeArgs]
         ($args)
         (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` ((
           $capturedArgs) => T))
         (implicit
             ev0: LocalPeer[L],
             ev1: PlacingTypes[P, T, U],
             ev2: PeerConnection[L#Connection, P, _]): U placed P = `#macro`
        """

        val issuedUsingExpr = s"""def using[T, U, I, L <: Peer, $typeArgs]
          ($args)
          (f: CurrentLocalPeerRemoteComputation[P] `implicit =>` ((
            $capturedArgs) => T))
          (implicit
              ev0: LocalPeer[L],
              ev1: PlacingTypes[P, T, I],
              ev2: IssuingTypes[L, I, U],
              ev3: PeerConnection[L#Connection, P, _],
              ev4: PeerConnection[P#Connection, L, _]): U placed P = `#macro`
        """

        (usingExpr, issuedUsingExpr)
      }

      val (usingExprs, issuedUsingExprs) = exprs.unzip

      val files = Map(
        dir / "retier" / "RemoteExpressionUsing.scala" ->
        s"""package retier

          import dslparadise._
          import scala.language.higherKinds

          protected trait RemoteExpressionUsing[P <: Peer, placed[_, _ <: Peer]] {
            ${usingExprs.mkString}
          }
        """,

        dir / "retier" / "RemoteExpressionIssuedUsing.scala" ->
        s"""package retier

          import dslparadise._
          import scala.language.higherKinds

          protected trait RemoteExpressionIssuedUsing[P <: Peer, placed[_, _ <: Peer]] {
            ${issuedUsingExprs.mkString}
          }
        """
      )

      files foreach { case (file, content) => IO write (file, content) }
      files.keys.toSeq
    }
  )
}
