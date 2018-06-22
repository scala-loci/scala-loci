package loci
package impl
package engine.generators

import engine._
import retypecheck.ReTyper
import scala.reflect.macros.blackbox.Context

trait Generation {
  val c: Context
  import c.universe._

  private[this] def root = termNames.ROOTPKG

  private[this] implicit class TypeAndSymbolSelection(tpe: Type) {
    def |(name: Name): Symbol = tpe member name
    def |(name: TypeName): Type = {
      val typeSymbol = (tpe member name).asType

      val typeInPackageObject = typeSymbol.toType match {
        case TypeRef(_, sym, args) =>
          val packageObject = tpe member termNames.PACKAGE
          if (packageObject.isModule) {
            val packageType = packageObject.asModule.moduleClass.asType.toType
            if ((packageType member name) == typeSymbol)
              Some(internal typeRef (packageType, sym, args))
            else
              None
          }
          else
            None
        case _ =>
          None
      }

      internal existentialAbstraction (
        typeSymbol.typeParams,
        typeInPackageObject getOrElse typeSymbol.toType)
    }
  }

  object types {
    val loci = (c.mirror staticPackage s"$root.loci").typeSignature

    val bottom = Seq(definitions.NothingTpe, definitions.NullTpe)

    val peer = typeOf[Peer]
    val peerTypeTag = typeOf[PeerTypeTag[_]]

    val remoteValue = typeOf[RemoteValue]

    val localOn = typeOf[_ localOn _]
    val sharedOn = typeOf[_ sharedOn _]
    val subjective = typeOf[_ <-> _]
    val subjectiveControlled = typeOf[_ <=> _]

    val functionPlacing = Seq(typeOf[_ => _], typeOf[(_ => _) localOn _])
    val controlledSubjectivePlacing = Seq(typeOf[_ <=> _], typeOf[(_ <=> _) localOn _])
    val subjectivePlacing = Seq(typeOf[_ <-> _], typeOf[(_ <-> _) localOn _])

    val from = typeOf[_ from _]
    val fromSingle = typeOf[_ fromSingle _]
    val fromMultiple = typeOf[_ fromMultiple _]
    val selection = Seq(from, fromSingle, fromMultiple)

    val transmissionProvider = typeOf[transmitter.TransmissionProvider]

    val connectionExpression = typeOf[RemoteConnectionExpression.type]
    val connectionInterface = typeOf[RemoteConnectionInterface]

    val multiple = typeOf[Peer#Multiple[_]]
    val optional = typeOf[Peer#Optional[_]]
    val single = typeOf[Peer#Single[_]]

    val multitier = typeOf[_root_.loci.multitier.type]

    val placing = loci | TypeName("PlacingExpression")
    val specialPlacing = loci | TypeName("SpecialPlacingExpression")
    val subjectivization = loci | TypeName("SubjectiveExpression")
    val overriding = loci | TypeName("OverridingExpression")

    val remote = loci | TypeName("RemoteExpression")
    val remoteSelection = loci | TypeName("RemoteSelectionExpression")
    val remoteSetting = loci | TypeName("RemoteSettingExpression")
    val remoteSubjectivization = loci | TypeName("RemoteSubjectiveExpression")
    val remoteCapturing = loci | TypeName("RemoteCapturingExpression")
    val remoteSubjectivizationCapturing = loci | TypeName("RemoteSubjectiveCapturingExpression")

    val fromExpression = sharedOn.companion | TypeName("FromExpression")
    val toExpression = localOn.companion | TypeName("ToExpression")
  }

  object symbols {
    val placedAbstract = types.specialPlacing | TermName("abstract")
    val placedBase = types.specialPlacing | TermName("base")
    val placedMain = types.specialPlacing | TermName("main")
    val placedTerminating = types.specialPlacing | TermName("terminating")
    val placedError = types.specialPlacing | TermName("error")
    val placedFatal = types.specialPlacing | TermName("fatal")
    val placedApply = types.placing | TermName("apply")
    val placedShared = types.placing | TermName("shared")
    val placedLocal = types.placing | TermName("local")
    val placedSubjectiveApply = types.subjectivization | TermName("apply")
    val placedOverriding = types.overriding | TermName("overriding")

    val placed = Seq(placedAbstract, placedMain, placedTerminating, placedError,
      placedFatal, placedApply, placedShared, placedLocal, placedSubjectiveApply)

    val specialPlaced = Map(
      placedMain -> names.main,
      placedTerminating -> names.terminating,
      placedError -> names.error,
      placedFatal -> names.fatal)

    val remoteApply = types.remote | TermName("apply")
    val remoteCall = types.remote | TermName("call")
    val remoteOn = (types.remoteSelection | TermName("on")).alternatives
    val remoteSet = types.remoteSetting | TermName(":=").encodedName
    val remoteSubjectiveApply = types.remoteSubjectivization | TermName("apply")
    val remoteCaptureApply = types.remoteCapturing | TermName("apply")
    val remoteSubjectiveCaptureApply = types.remoteSubjectivizationCapturing | TermName("apply")

    val remote = Seq(remoteSubjectiveApply, remoteSubjectiveCaptureApply, remoteApply, remoteCaptureApply)

    val running = types.multitier | TermName("running")
    val terminate = types.multitier | TermName("terminate")

    val fromExpression = types.sharedOn.companion | TermName("FromExpression")
    val toExpression = types.localOn.companion | TermName("ToExpression")

    val transmitMultiple = types.remoteValue.companion | TermName("transmitMultiple")
    val transmitOptional = types.remoteValue.companion | TermName("transmitOptional")
    val transmitSingle = types.remoteValue.companion | TermName("transmitSingle")

    val transmit = Seq(transmitMultiple, transmitOptional, transmitSingle)

    val multipleConnection = types.connectionExpression | TermName("multipleConnection")
    val optionalConnection = types.connectionExpression | TermName("optionalConnection")
    val singleConnection = types.connectionExpression | TermName("singleConnection")

    val connection = Seq(multipleConnection, optionalConnection, singleConnection)

    val peer = types.loci | TermName("peer")
    val placedValue = types.loci | TermName("lociPlacedValue")
    val localPlacedValue = types.loci | TermName("lociLocalPlacedValue")

    val discardValue = types.localOn.companion | TermName("discardValue")
    val subjectivizeValue = types.localOn.companion | TermName("subjectivizeValue")
    val subjectivizeValueControlled = types.localOn.companion | TermName("subjectivizeValueControlled")

    val lociLiftLocalPlacedValueGlobally = types.loci | TermName("lociLiftLocalPlacedValueGlobally")
    val lociLiftLocalPlacedValueLocally = types.loci | TermName("lociLiftLocalPlacedValueLocally")
    val downcastValueGlobally = types.localOn.companion | TermName("downcastValueGlobally")
    val downcastValueLocally = types.localOn.companion | TermName("downcastValueLocally")

    val globalCasts = Seq(discardValue, subjectivizeValue, subjectivizeValueControlled,
      lociLiftLocalPlacedValueGlobally, downcastValueGlobally)
    val localCasts = Seq(placedValue, localPlacedValue, lociLiftLocalPlacedValueLocally, downcastValueLocally)
    val casts = globalCasts ++ localCasts
  }

  object names {
    val from = TermName("from")
    val to = TermName("to")
    val peerTypeTag = TermName("peerTypeTag")
    val peerType = TermName("peerType")
    val Tie = TermName("Tie")
    val createPeerSelection = TermName("createPeerSelection")
    val createMultipleTransmission = TermName("createMultipleTransmission")
    val createOptionalTransmission = TermName("createOptionalTransmission")
    val createSingleTransmission = TermName("createSingleTransmission")
    val executeTransmission = TermName("executeTransmission")
    val createMultipleRemoteConnection = TermName("createMultipleRemoteConnection")
    val createOptionalRemoteConnection = TermName("createOptionalRemoteConnection")
    val createSingleRemoteConnection = TermName("createSingleRemoteConnection")
    val system = lociTermName("system")
    val metapeer = lociTermName("metapeer")
    val systemMain = TermName("main")
    val systemRunning = TermName("running")
    val systemTerminate = TermName("terminate")
    val implementation = lociTypeName("peer")
    val interface = lociTermName("peer")
    val dispatch = lociTermName("dispatch")
    val main = lociTermName("main")
    val terminating = lociTermName("terminating")
    val error = lociTermName("error")
    val fatal = lociTermName("fatal")
  }

  object trees {
    val Try = tq"$root.scala.util.Try"
    val TryCreate = q"$root.scala.util.Try"
    val implicitly = q"$root.scala.Predef.implicitly"
    val Success = q"$root.scala.util.Success"
    val Failure = q"$root.scala.util.Failure"
    val List = q"$root.scala.collection.immutable.List"
    val Map = q"$root.scala.collection.immutable.Map"
    val compileTimeOnly = tq"$root.scala.annotation.compileTimeOnly"
    val multitier = q"$root.loci.multitier"
    val multitierAnnotation = q"new $root.loci.multitier"
    val peerTypeOf = q"$root.loci.peerTypeOf"
    val MessageBuffer = tq"$root.loci.MessageBuffer"
    val MessageBufferEmpty = q"$root.loci.MessageBuffer.empty"
    val AbstractionId = tq"$root.loci.transmitter.AbstractionId"
    val AbstractionIdCreate = q"$root.loci.impl.AbstractionId.create"
    val AbstractionRef = tq"$root.loci.transmitter.AbstractionRef"
    val AbstractionRefOps = tq"$root.loci.impl.AbstractionRef.AbstractionRefOps"
    val RemoteRefOps = tq"$root.loci.impl.RemoteRef.RemoteRefOps"
    val Marshallable = q"$root.loci.transmitter.Marshallable"
    val MarshallableArgument = q"$root.loci.transmitter.Marshallable.Argument"
    val UnitMarshallable = q"$root.loci.impl.UnitMarshallable"
    val Remote = tq"$root.loci.Remote"
    val Peer = q"$root.loci.Peer"
    val PeerType = tq"$root.loci.PeerType"
    val PeerTypeTag = tq"$root.loci.PeerTypeTag"
    val PeerTypeTagCreate = q"$root.loci.impl.PeerTypeTag.create"
    val PeerImpl = tq"$root.loci.impl.PeerImpl"
    val throwMetaPeerNotSetUp = q"$root.loci.impl.PeerImpl.throwMetaPeerNotSetUp"
    val TieMultiplicity = tq"$root.loci.impl.TieMultiplicity"
    val SingleTie = q"$root.loci.impl.SingleTie"
    val OptionalTie = q"$root.loci.impl.OptionalTie"
    val MultipleTie = q"$root.loci.impl.MultipleTie"
    val TransmissionProperties = tq"$root.loci.impl.TransmissionProperties"
    val TransmissionPropertiesCreate = q"$root.loci.impl.TransmissionProperties.create"
    val System = tq"$root.loci.impl.System"
    val Runtime = q"$root.loci.impl.Runtime"
  }


  trait Statement { def index: Int }

  case class EnclosingContext(name: TypeName, bases: List[Tree])

  case class InputStatement(stat: Tree, index: Int) extends Statement

  case class PeerDefinition(tree: Tree, peerSymbol: TypeSymbol,
    typeArgs: List[Tree], args: List[List[Tree]], parents: List[Tree],
    mods: Modifiers, stats: List[Tree], isClass: Boolean,
    companion: Option[Tree], index: Int) extends Statement

  case class PlacedStatement(tree: Tree, peerSymbol: TypeSymbol, exprType: Type,
    declTypeTree: Option[Tree], overridingDecl: Option[TermSymbol], expr: Tree,
    index: Int) extends Statement

  case class NonPlacedStatement(tree: Tree, index: Int) extends Statement

  case class PlacedAbstraction(peerSymbol: TypeSymbol,
    interfaceDefinitions: List[Tree], dispatchClause: CaseDef)

  case class PeerTieMultiplicity(peerSymbol: TypeSymbol, tiedPeer: Tree,
    tieMultiplicity: Tree)

  case class OutputStatement(stat: Tree, index: Int) extends Statement


  implicit class NonPlacedStatementOps(stat: NonPlacedStatement) {
    val isPeerBound = stat.tree match {
      case ValDef(_, _, _, _)
          if !stat.tree.isLociSynthetic && stat.tree.symbol == NoSymbol =>
        true
      case _ =>
        stat.tree.symbol != null &&
        stat.tree.symbol != NoSymbol &&
        !stat.tree.symbol.isType &&
        !stat.tree.symbol.asTerm.isParamAccessor &&
        stat.tree.symbol.name != termNames.CONSTRUCTOR
    }
  }


  val echo = Echo(c)

  val typer = ReTyper(c)


  def lociTermName(name: String) = TermName(s"$$$$loci$$$name")

  def lociTypeName(name: String) = TypeName(s"$$$$loci$$$name")

  def lociName(name: TermName) = TermName(s"$$$$loci$$${name.toString}")

  def lociName(name: TypeName) = TypeName(s"$$$$loci$$${name.toString}")

  implicit class NameOps(val name: Name) {
    def isLociName = name.toString startsWith "$$loci$"
  }


  private case object LociSynthetic

  def markLociSynthetic(tree: Tree, pos: Position): tree.type =
    markLociSynthetic(tree, pos, true)

  def markLociSynthetic(tree: Tree, pos: Position, recursive: Boolean)
  : tree.type = {
    if (recursive)
      tree foreach { tree =>
        if (tree.pos == NoPosition)
          internal setPos (tree, pos)
      }
    else if (tree.pos == NoPosition)
      internal setPos (tree, pos)

    markLociSynthetic(tree)
  }

  def markLociSynthetic(tree: Tree): tree.type =
    markLociSynthetic(tree, true)

  def markLociSynthetic(tree: Tree, recursive: Boolean): tree.type = {
    if (recursive)
      tree foreach { internal updateAttachment (_, LociSynthetic) }
    else
      internal updateAttachment (tree, LociSynthetic)
    tree
  }

  def unmarkLociSynthetic(tree: Tree): tree.type = {
    tree foreach { internal removeAttachment[LociSynthetic.type] _ }
    tree
  }

  implicit class TreeOps(val tree: Tree) {
    def isLociSynthetic: Boolean =
      (internal attachments tree).get[LociSynthetic.type].nonEmpty

    def typeTree: Tree = typeTree(false)
    def typeTree(abortOnFailure: Boolean): Tree = tree match {
      case tree: TypeTree if tree.original != null =>
        internal setType (tree.original, tree.tpe)

      case _ if tree.tpe != null =>
        (typer createTypeTree (tree.tpe, tree.pos)) match {
          case _: TypeTree if abortOnFailure =>
            typeTreeGenerationFailed(tree.pos, tree.tpe)
          case createdTree =>
            internal setType (createdTree, tree.tpe)
        }

      case _ if abortOnFailure =>
        typeTreeGenerationFailed(tree.pos, tree)

      case _ =>
        tree
    }

    def typeArgTrees: List[Tree] = typeArgTrees(false)
    def typeArgTrees(abortOnFailure: Boolean): List[Tree] = {
      val args = tree.typeTree(abortOnFailure) match {
        case AppliedTypeTree(_, args) => args
        case _ => tree.tpe.underlying.typeArgs map TypeTree
      }
      (args zip tree.tpe.underlying.typeArgs) map { case (tree, tpe) =>
        internal setType (tree.typeTree(abortOnFailure), tpe)
      }
    }

    private[this] def typeTreeGenerationFailed(pos: Position, any: Any) =
      c.abort(pos, s"failed to generate type tree for $any")
  }


  implicit class TypeOps(tpe: Type) {
    def =:!=(that: Type): Boolean = !(tpe =:= that)
    def <:!<(that: Type): Boolean = !(tpe <:< that)
    def isGeneric: Boolean = tpe exists { _.typeSymbol.isParameter }
    def underlying: Type =
      if (tpe ne tpe.dealias)
        tpe.dealias.underlying
      else if (tpe ne tpe.widen)
        tpe.widen.underlying
      else
        tpe
  }


  def wildcardedTypeTree(expr: Tree, typeArgsCount: Int) =
    if (typeArgsCount == 0)
      expr
    else {
      val wildcards = ((0 until typeArgsCount) map { _ =>
        TypeName(c freshName "_")
      }).toList

      ExistentialTypeTree(
        AppliedTypeTree(expr, wildcards map { Ident(_) }),
        wildcards map { TypeDef(
          Modifiers(Flag.DEFERRED | Flag.SYNTHETIC), _, List.empty,
          TypeBoundsTree(EmptyTree, EmptyTree))
        })
    }


  def peerImplementationTree(baseTree: Tree, peerType: Type,
      peerSymbolsUnderExpansion: List[TypeSymbol]): Tree =
    peerGeneratedComponentTree(
      baseTree, peerType, peerSymbolsUnderExpansion,
      names.implementation, "implementation")

  def peerInterfaceTree(baseTree: Tree, peerType: Type,
      peerSymbolsUnderExpansion: List[TypeSymbol]): Tree =
    peerGeneratedComponentTree(
      baseTree, peerType, peerSymbolsUnderExpansion,
      names.interface, "interface")

  def peerTypeTagTree(baseTree: Tree, peerType: Type,
      peerSymbolsUnderExpansion: List[TypeSymbol]): Tree =
    peerGeneratedComponentTree(
      baseTree, peerType, peerSymbolsUnderExpansion,
      names.peerTypeTag, "tag")

  private[this] def peerGeneratedComponentTree(baseTree: Tree, peerType: Type,
      peerSymbolsUnderExpansion: List[TypeSymbol],
      propName: Name, propMessage: String): Tree = {
    if (!(peerSymbolsUnderExpansion contains peerType.typeSymbol) &&
        (peerType.dealias.companion member propName) == NoSymbol)
        c.abort(baseTree.pos,
          s"cannot access peer type $propMessage " +
          s"(maybe peer definition was not placed " +
          s"inside `multitier` environment)")

    val companion = peerType.dealias.typeSymbol.name.toTermName

    val expr = baseTree match {
      case q"$expr.$_[..$_](...$_)" => expr
      case tq"$expr.$_[..$_]" => expr
      case tq"$expr.$_[..$_] forSome { ..$_ }" => expr
      case _ =>
        (typer createTypeTree (peerType, baseTree.pos)) match {
          case tq"$expr.$_[..$_]" => expr
          case tq"$expr.$_[..$_] forSome { ..$_ }" => expr
        }
    }

    propName match {
      case propName: TermName => q"$expr.$companion.$propName"
      case propName: TypeName => tq"$expr.$companion.$propName"
    }
  }
}
