import sbt._
import Keys._

object SourceGenerator {
  val transmittableTuples =
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val members = (1 to 22) map { i =>
        val tuple = s"Tuple$i"
        val tupleArgsT = (0 until i) map { i => s"T$i" } mkString ", "
        val tupleArgsS = (0 until i) map { i => s"S$i" } mkString ", "
        val tupleArgsR = (0 until i) map { i => s"R$i" } mkString ", "

        val typeArgs = (0 until i) map { i => s"T$i, S$i, R$i" } mkString ", "

        val typeArgsIdentically = (0 until i) map { i => s"""
          |      T$i: IdenticallyTransmittable""" } mkString ","

        val implicitArgs = (0 until i) map { i => s"""
          |      transmittable$i: Transmittable[T$i, S$i, R$i]""" } mkString ","

        val send = (0 until i) map { i => s"""
          |          transmittable$i send value._${i+1}""" } mkString ","

        val receive = (0 until i) map { i => s"""
          |          transmittable$i receive value._${i+1}""" } mkString ","

        val tupleMember = s"""
          |  implicit def tuple$i[$typeArgs](implicit $implicitArgs)
          |    : Transmittable[
          |      $tuple[$tupleArgsT],
          |      $tuple[$tupleArgsS],
          |      $tuple[$tupleArgsR]] =
          |    new PullBasedTransmittable[
          |        $tuple[$tupleArgsT],
          |        $tuple[$tupleArgsS],
          |        $tuple[$tupleArgsR]] {
          |      def send(value: $tuple[$tupleArgsT], remote: RemoteRef) =
          |        if (value == null) null
          |        else $tuple($send)
          |      def receive(value: $tuple[$tupleArgsS], remote: RemoteRef) =
          |        if (value == null) null
          |        else $tuple($receive)
          |    }
          |"""

        val identicalTupleMember = s"""
          |  implicit def identicalTuple$i[$typeArgsIdentically] =
          |    IdenticallyTransmittable[$tuple[$tupleArgsT]]
          |"""

        (tupleMember, identicalTupleMember)
      }

      val (tupleMembers, identicalTupleMembers) = members.unzip

      val files = Map(
        dir / "retier" / "transmission" / "TransmittableTuples.scala" ->
        s"""package retier
           |package transmission
           |
           |trait TransmittableGeneralTuples extends TransmittableIdentity {
           |${tupleMembers.mkString}
           |}
           |
           |trait TransmittableTuples extends TransmittableGeneralTuples {
           |${identicalTupleMembers.mkString}
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO write (file, content) }
      files.keys.toSeq
    }

  val valueTypesHigherKinds =
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val higherKinds = (1 to 8) map { i =>
        val typeArgsT = (0 until i) map { i => "_" } mkString ", "
        val typeArgsU = (0 until i) map { i => s"U$i" } mkString ", "
        val typeArgsV = (0 until i) map { i => s"V$i" } mkString ", "
        val typeArgsDummy = (0 until i) map { i => s"Dummy$i" } mkString ", "

        val higherKindArgs = (0 until i) map { i => s"""
          |        ev${i+1}: ValueTypes[U$i, _, Dummy$i, V$i]""" } mkString ","

        val typeArgs =
          s"T[$typeArgsT], $typeArgsU, $typeArgsV, $typeArgsDummy"

        val resultType =
          s"ValueTypes[T[$typeArgsU], Nothing, T[$typeArgsV], T[$typeArgsV]]"

        val higherKind = s"""
          |  implicit def higherKind$i[$typeArgs]
          |    (implicit
          |        ev0: NotNothing[T[$typeArgsU]], $higherKindArgs)
          |    : $resultType = `#macro`
          |"""

        higherKind
      }

      val files = Map(
        dir / "retier" / "ValueTypesHigherKinds.scala" ->
        s"""package retier
           |
           |import scala.language.higherKinds
           |
           |trait ValueTypesHigherKinds extends ValueTypesIdentity {
           |${higherKinds.mkString}
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO write (file, content) }
      files.keys.toSeq
    }

  val signalDefaultTuples =
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val tuples = (1 to 22) map { i =>
        val args = (0 until i) map { i => s"default[T$i]" } mkString ", "
        val typeArgs = (0 until i) map { i => s"""
          |        T$i: SignalDefaultValue""" } mkString ", "

        val tuple = s"""
          |    implicit def defaultTuple$i[$typeArgs] =
          |      SignalDefaultValue(Tuple$i($args))"""

        tuple
      }

      val files = Map(
        dir / "retier" / "SignalDefaultTuples.scala" ->
        s"""package retier
           |
           |trait SignalDefaultTuples { this: SignalDefaultValues =>
           |  trait SignalDefaultTupleValues extends BasicSignalDefaultValues {
           |${tuples.mkString}
           |  }
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO write (file, content) }
      files.keys.toSeq
    }
}
