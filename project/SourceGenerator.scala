import sbt._
import sbt.Keys._

object SourceGenerator {
  val transmittableTuples =
    Compile / sourceGenerators += Def.taskDyn {
      val scala3 = scalaVersion.value startsWith "3."

      def baseSelector(index: Int) =
        if (scala3) s"using Selector.Base(using null, ValueOf($index))" else s"Selector.unchecked($index)"

      def intermediateSelector(index: Int) =
        if (scala3) s"using Selector.Intermediate(using null, ValueOf($index))" else s"Selector.unchecked($index)"

      def condition(cond: String, thenCase: String, elseCase: String) =
        if (scala3) s"if $cond then $thenCase else $elseCase" else s"if ($cond) $thenCase else $elseCase"

      def refinementType(refinementType: String) =
        if (scala3) s"($refinementType)" else refinementType

      val implicitDefinition =
        if (scala3) s"given" else s"final implicit def"

      val implicitArgument =
        if (scala3) s"using" else s"implicit"

      val generalGenericTuples =
        if (scala3) " TransmittableGeneralGenericTuples with" else ""

      val genericTuples =
        if (scala3) " TransmittableGenericTuples with" else ""

      val imports =
        if (scala3) """
          |  import scala.language.unsafeNulls"""
        else
          ""

      Compile / sourceManaged map { dir =>
        val members = (1 to 22) map { i =>
          val tuple = s"Tuple$i"
          val tupleArgsT = (0 until i) map { i => s"T$i" } mkString ", "
          val tupleArgsB = (0 until i) map { i => s"B$i" } mkString ", "
          val tupleArgsI = (0 until i) map { i => s"I$i" } mkString ", "
          val tupleArgsR = (0 until i) map { i => s"R$i" } mkString ", "

          val tupleArgs = s"$tuple[$tupleArgsB], $tuple[$tupleArgsI], $tuple[$tupleArgsR]"

          val typeArgs = (0 until i) map { i => s"B$i, I$i, R$i" } mkString ", "

          val typeArgsIdentically = (0 until i) map { i => s"""
          |      T$i: IdenticallyTransmittable""" } mkString ","

          val implicitArgs = (0 until i) map { i => s"""
          |      transmittable$i: Transmittable[B$i, I$i, R$i]""" } mkString ","

          val delegatesType = (0 until i) map { i =>
            s"transmittable$i.Type"
          } mkString " / "

          val baseDelegates = (0 until i) map { k => s"""
          |        context.delegate(value._${k+1})(${baseSelector(i-k-1)})"""
          } mkString ","

          val intermediateDelegates = (0 until i) map { k => s"""
          |        context.delegate(value._${k+1})(${intermediateSelector(i-k-1)})"""
          } mkString ","

          val baseDelegation = condition("value == null", "null", s"$tuple($baseDelegates)")

          val intermediateDelegation = condition("value == null", "null", s"$tuple($intermediateDelegates)")

          val tupleMemberType = refinementType(
            s"""DelegatingTransmittable[$tupleArgs] {
              |      type Delegates = $delegatesType
              |    }""")

          val tupleMember = s"""
            |  $implicitDefinition tuple$i[$typeArgs]($implicitArgument $implicitArgs)
            |  : $tupleMemberType =
            |    DelegatingTransmittable(
            |      provide = (value, context) => $baseDelegation,
            |      receive = (value, context) => $intermediateDelegation)
            |"""

          val identicalTupleMember = s"""
            |  $implicitDefinition identicalTuple$i[$typeArgsIdentically]
            |  : IdenticallyTransmittable[$tuple[$tupleArgsT]] =
            |    IdenticallyTransmittable()
            |"""

          (tupleMember, identicalTupleMember)
        }

        val (tupleMembers, identicalTupleMembers) = members.unzip

        val files = Map(
          dir / "loci" / "transmitter" / "TransmittableTuples.scala" ->
            s"""package loci
               |package transmitter
               |
               |trait TransmittableGeneralTuples extends$generalGenericTuples TransmittableDummy {
               |  this: Transmittable.Base =>
               |$imports
               |${tupleMembers.mkString}
               |}
               |
               |trait TransmittableTuples extends$genericTuples TransmittableGeneralTuples {
               |  this: Transmittable.Base =>
               |${identicalTupleMembers.mkString}
               |}
               |""".stripMargin
        )

        files foreach { case (file, content) => IO.write(file, content) }
        files.keys.toSeq
      }
    }

  val remoteSelection =
    Compile / sourceGenerators += Compile / sourceManaged map { dir =>
      val members = (2 to 22) map { i =>
        val args = s"Remote[R]${", Remote[R]" * (i - 1)}"
        s"""  implicit def tuple$i[R](r: Tuple$i[$args]): RemoteSelection[R, fromMultiple] = erased
        |"""
      }

      val files = Map(
        dir / "loci" / "embedding" / "RemoteSelection.scala" ->
        s"""package loci
           |package embedding
           |
           |import loci.language.Remote
           |
           |import scala.language.implicitConversions
           |
           |sealed trait RemoteSelection[R, placed[_, _]]
           |
           |object RemoteSelection {
           |  implicit def tuple1[R](r: Tuple1[Remote[R]]): RemoteSelection[R, fromSingle] = erased
           |${members.mkString}
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO.write(file, content) }
      files.keys.toSeq
    }

  val functionsBindingBuilder =
    Compile / sourceGenerators += Compile / sourceManaged map { dir =>
      val builders = (0 to 22) map { i =>
        val argTypes = (0 until i) map { i => s"T$i" } mkString ", "
        val typedArgs = (0 until i) map { i => s"v$i: T$i" } mkString ", "
        val args = (0 until i) map { i => s"v$i" } mkString ", "

        val function =
          if (i == 0) s"function$i[R, P]"
          else s"function$i[$argTypes, R, P]"

        val marshallables =
          if (i == 0) s"""
            |      res: Marshallable[R, _, P]"""
          else s"""
            |      arg: Marshallable[($argTypes), ($argTypes), _],
            |      res: Marshallable[R, _, P]"""

        val marshalling =
          if (i == 0) "MessageBuffer.empty"
          else s"arg.marshal(($args), abstraction)"

        val tupledFunction =
          if (i < 2) s"function(abstraction.remote)"
          else s"function(abstraction.remote).tupled"

        val dispatch =
          if (i == 0) s"""
            |          Try { res.marshal($tupledFunction(), abstraction) }"""
          else s"""
            |          arg.unmarshal(message, abstraction) map { arg =>
            |            res.marshal($tupledFunction(arg), abstraction) }"""

        s"""
          |  implicit def $function(implicit $marshallables)
          |      : BindingBuilder[($argTypes) => R, ($argTypes) => P] = {
          |    new BindingBuilder[($argTypes) => R, ($argTypes) => P] {
          |      def apply(bindingName: String) = new Binding[($argTypes) => R, ($argTypes) => P] {
          |        val name = bindingName
          |        def dispatch(
          |            function: RemoteRef => ($argTypes) => R,
          |            message: MessageBuffer,
          |            abstraction: AbstractionRef) = $dispatch
          |        def call[A <: AbstractionRef](
          |            createAbstraction: () => A)(
          |            handler: (MessageBuffer, A) => Notice.Steady[Try[MessageBuffer]]) = {
          |          ($typedArgs) =>
          |            val abstraction = createAbstraction()
          |            res.unmarshal(handler($marshalling, abstraction), abstraction)
          |        }
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
           |import loci.transmitter.{AbstractionRef, Marshallable, RemoteRef}
           |
           |import scala.util.Try
           |
           |trait FunctionsBindingBuilder extends ValueBindingBuilder {
           |${builders.mkString}
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO.write(file, content) }
      files.keys.toSeq
    }

  val functionSubjectiveBinding =
    Compile / sourceGenerators += Compile / sourceManaged map { dir =>
      val subjectiveBinding = (0 to 21) map { i =>
        val argTypes = (0 until i) map { i => s"T$i" } mkString ", "
        val typedArgs = (0 until i) map { i => s"v$i: T$i" } mkString ", "
        val args = (0 until i) map { i => s"v$i" } mkString ", "

        val function =
          if (i == 0) s"function${i + 1}[R]"
          else s"function${i + 1}[$argTypes, R]"

        val application =
          if (i == 0) s"function(remote)"
          else s"function(remote, $args)"

        val input =
          if (i == 0) s"(RemoteRef) => R"
          else s"(RemoteRef, $argTypes) => R"

        val output = s"($argTypes) => R"

        s"""
          |  implicit def $function
          |      : SubjectiveBinding[$input, $output] =
          |    new SubjectiveBinding[$input, $output] {
          |      def apply(remote: RemoteRef, function: $input) =
          |        ($typedArgs) => $application
          |    }
          |"""
      }

      val files = Map(
        dir / "loci" / "registry" / "FunctionSubjectiveBinding.scala" ->
        s"""package loci
           |package registry
           |
           |import loci.transmitter.RemoteRef
           |
           |trait FunctionSubjectiveBinding extends ValueSubjectiveBinding {
           |${subjectiveBinding.mkString}
           |}
           |""".stripMargin
      )

      files foreach { case (file, content) => IO.write(file, content) }
      files.keys.toSeq
    }
}
