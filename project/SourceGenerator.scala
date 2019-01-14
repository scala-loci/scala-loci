import sbt._
import Keys._

object SourceGenerator {
  val transmittableTuples =
    sourceGenerators in Compile += sourceManaged in Compile map { dir =>
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
        dir / "loci" / "transmitter" / "TransmittableTuples.scala" ->
        s"""package loci
           |package transmitter
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

  val functionsBindingBuilder =
    sourceGenerators in Compile += sourceManaged in Compile map { dir =>
      val builders = (0 to 22) map { i =>
        val argTypes = (0 until i) map { i => s"T$i" } mkString ", "
        val typedArgs = (0 until i) map { i => s"v$i: T$i" } mkString ", "
        val args = (0 until i) map { i => s"v$i" } mkString ", "

        val function =
          if (i == 0) s"function$i[R]"
          else s"function$i[$argTypes, R]"

        val marshallables =
          if (i == 0) s"""
            |      res: Marshallable[R]"""
          else s"""
            |      arg: MarshallableArgument[($argTypes)],
            |      res: Marshallable[R]"""

        val marshalling =
          if (i == 0) "MessageBuffer.empty"
          else s"arg marshal (($args), abstraction)"

        val tupledFunction =
          if (i < 2) s"function(abstraction.remote)"
          else s"function(abstraction.remote).tupled"

        val dispatch =
          if (i == 0) s"""
            |          Try { res marshal ($tupledFunction(), abstraction) }"""
          else s"""
            |          arg unmarshal (message, abstraction) map { arg =>
            |            res marshal ($tupledFunction(arg), abstraction) }"""

        s"""
          |  implicit def $function(implicit $marshallables) = {
          |    new BindingBuilder[($argTypes) => R] {
          |      type RemoteCall = ($argTypes) => Future[res.Result]
          |      def apply(bindingName: String) = new Binding[($argTypes) => R] {
          |        type RemoteCall = ($argTypes) => Future[res.Result]
          |        val name = bindingName
          |        def dispatch(
          |            function: RemoteRef => ($argTypes) => R, message: MessageBuffer,
          |            abstraction: AbstractionRef) = $dispatch
          |        def call(
          |            abstraction: AbstractionRef)(
          |            handler: Binding.Handler) =
          |          ($typedArgs) =>
          |            createCall(handler, $marshalling, res, abstraction)
          |      }
          |    }
          |  }
          |"""
      }

      val files = Map(
        dir / "loci" / "registry" / "FunctionsBindingBuilder.scala" ->
        s"""package loci
           |package registry
           |
           |import transmitter.AbstractionRef
           |import transmitter.Marshallable
           |import transmitter.MarshallableArgument
           |import transmitter.RemoteRef
           |import scala.util.Try
           |import scala.concurrent.Future
           |
           |trait FunctionsBindingBuilder extends ValueBindingBuilder {
           |${builders.mkString}
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO write (file, content) }
      files.keys.toSeq
    }

  val valueTypesHigherKinds =
    sourceGenerators in Compile += sourceManaged in Compile map { dir =>
      val higherKinds = (1 to 8) map { i =>
        val typeArgsT = (0 until i) map { i => "_" } mkString ", "
        val typeArgsU = (0 until i) map { i => s"U$i" } mkString ", "
        val typeArgsV = (0 until i) map { i => s"V$i" } mkString ", "
        val typeArgsDummy = (0 until i) map { i => s"Dummy$i" } mkString ", "

        val higherKindEvidences = (0 to i) map { i => s"ev$i" } mkString ", "

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
          |    : $resultType = `#macro`($higherKindEvidences)
          |"""

        higherKind
      }

      val files = Map(
        dir / "loci" / "ValueTypesHigherKinds.scala" ->
        s"""package loci
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
}
