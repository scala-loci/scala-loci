package retier
package impl
package engine.generators

import engine._
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
    val retier = (c.mirror staticPackage s"$root.retier").typeSignature

    val bottom = Seq(definitions.NothingTpe, definitions.NullTpe)

    val peer = typeOf[Peer]
    val peerTypeTag = typeOf[PeerTypeTag[_]]

    val localOn = typeOf[_ localOn _]
    val sharedOn = typeOf[_ sharedOn _]
    val issued = typeOf[_ <-> _]
    val issuedControlled = typeOf[_ <=> _]

    val functionPlacing = Seq(typeOf[_ => _], typeOf[(_ => _) localOn _])
    val controlledIssuedPlacing = Seq(typeOf[_ <=> _], typeOf[(_ <=> _) localOn _])
    val issuedPlacing = Seq(typeOf[_ <-> _], typeOf[(_ <-> _) localOn _])

    val from = typeOf[_ from _]
    val fromSingle = typeOf[_ fromSingle _]
    val fromMultiple = typeOf[_ fromMultiple _]
    val selection = Seq(from, fromSingle, fromMultiple)

    val transmissionProvider = typeOf[transmission.TransmissionProvider]

    val multiple = typeOf[Peer#Multiple[_]]
    val optional = typeOf[Peer#Optional[_]]
    val single = typeOf[Peer#Single[_]]

    val placing = retier | TypeName("PlacingExpression")
    val issuing = retier | TypeName("IssuingExpression")
    val overriding = retier | TypeName("OverridingExpression")

    val remote = retier | TypeName("RemoteExpression")
    val remoteSelection = retier | TypeName("RemoteSelectionExpression")
    val remoteSetting = retier | TypeName("RemoteSettingExpression")
    val remoteIssuing = retier | TypeName("RemoteIssuingExpression")
    val remoteCapturing = retier | TypeName("RemoteCapturingExpression")
    val remoteIssuedCapturing = retier | TypeName("RemoteIssuedCapturingExpression")

    val fromExpression = retier | retierTypeName("FromExpression")
  }

  object symbols {
    val placedAbstract = types.placing | TermName("abstract")
    val placedBase = types.placing | TermName("base")
    val placedApply = types.placing | TermName("apply")
    val placedShared = types.placing | TermName("shared")
    val placedLocal = types.placing | TermName("local")
    val placedIssuedApply = types.issuing | TermName("apply")
    val placedOverriding = types.overriding | TermName("overriding")

    val placed = Seq(placedAbstract, placedApply, placedShared, placedLocal, placedIssuedApply)

    val remoteApply = types.remote | TermName("apply")
    val remoteCall = types.remote | TermName("call")
    val remoteOn = (types.remoteSelection | TermName("on")).alternatives
    val remoteSet = types.remoteSetting | TermName(":=").encodedName
    val remoteIssuedApply = types.remoteIssuing | TermName("apply")
    val remoteCaptureApply = types.remoteCapturing | TermName("apply")
    val remoteIssuedCaptureApply = types.remoteIssuedCapturing | TermName("apply")

    val remote = Seq(remoteIssuedApply, remoteIssuedCaptureApply, remoteApply, remoteCaptureApply)

    val fromExpression = types.retier | retierTermName("FromExpression")

    val transmitMultiple = types.retier | TermName("transmitMultiple")
    val transmitOptional = types.retier | TermName("transmitOptional")
    val transmitSingle = types.retier | TermName("transmitSingle")

    val transmit = Seq(transmitMultiple, transmitOptional, transmitSingle)

    val value = types.retier | retierTermName("value")

    val discardValue = types.retier | retierTermName("discardValue")
    val issueValue = types.retier | retierTermName("issueValue")
    val issueValueControlled = types.retier | retierTermName("issueValueControlled")

    val liftValueGlobally = types.retier | retierTermName("liftValueGlobally")
    val liftValueLocally = types.retier | retierTermName("liftValueLocally")
    val downcastValueGlobally = types.retier | retierTermName("downcastValueGlobally")
    val downcastValueLocally = types.retier | retierTermName("downcastValueLocally")

    val globalCasts = Seq(discardValue, issueValue, issueValueControlled,
      liftValueGlobally, downcastValueGlobally)
    val localCasts = Seq(value, liftValueLocally, downcastValueLocally)
    val casts = globalCasts ++ localCasts
  }

  object names {
    val from = TermName("from")
    val peerTypeTag = TermName("peerTypeTag")
    val peerType = TermName("peerType")
    val connection = TermName("Connection")
    val createPeerSelection = TermName("createPeerSelection")
    val createMultipleTransmission = TermName("createMultipleTransmission")
    val createOptionalTransmission = TermName("createOptionalTransmission")
    val createSingleTransmission = TermName("createSingleTransmission")
    val executeTransmission = TermName("executeTransmission")
    val system = retierTermName("system")
    val implementation = retierTypeName("peer")
    val interface = retierTermName("peer")
    val dispatch = retierTermName("dispatch")
  }

  object trees {
    val Try = tq"$root.scala.util.Try"
    val TryCreate = q"$root.scala.util.Try"
    val String = tq"$root.scala.Predef.String"
    val implicitly = q"$root.scala.Predef.implicitly"
    val Success = q"$root.scala.util.Success"
    val Failure = q"$root.scala.util.Failure"
    val List = q"$root.scala.collection.immutable.List"
    val Map = q"$root.scala.collection.immutable.Map"
    val compileTimeOnly = tq"$root.scala.annotation.compileTimeOnly"
    val peerTypeOf = q"$root.retier.peerTypeOf"
    val `#macro` = q"$root.retier.`#macro`"
    val AbstractionId = tq"$root.retier.transmission.AbstractionId"
    val AbstractionIdCreate = q"$root.retier.impl.AbstractionId.create"
    val AbstractionRef = tq"$root.retier.transmission.AbstractionRef"
    val Marshallable = q"$root.retier.transmission.Marshallable"
    val MarshallableArgument = q"$root.retier.transmission.MarshallableArgument"
    val UnitMarshallable = q"$root.retier.impl.UnitMarshallable"
    val Remote = tq"$root.retier.Remote"
    val Peer = q"$root.retier.Peer"
    val PeerType = tq"$root.retier.PeerType"
    val PeerTypeTag = tq"$root.retier.PeerTypeTag"
    val PeerTypeTagCreate = q"$root.retier.impl.PeerTypeTag.create"
    val PeerImpl = tq"$root.retier.impl.PeerImpl"
    val ConnectionMultiplicity = tq"$root.retier.impl.ConnectionMultiplicity"
    val SingleConnection = q"$root.retier.impl.SingleConnection"
    val OptionalConnection = q"$root.retier.impl.OptionalConnection"
    val MultipleConnection = q"$root.retier.impl.MultipleConnection"
    val TransmissionProperties = tq"$root.retier.impl.TransmissionProperties"
    val TransmissionPropertiesCreate = q"$root.retier.impl.TransmissionProperties.create"
    val System = tq"$root.retier.impl.System"
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

  case class PeerConnectionMultiplicity(peerSymbol: TypeSymbol,
    connectedPeer: Tree, connectionMultiplicity: Tree)

  case class OutputStatement(stat: Tree, index: Int) extends Statement


  val echo = Echo(c)

  val typer = Typer(c)


  def retierTermName(name: String) = TermName(s"$$$$retier$$$name")

  def retierTypeName(name: String) = TypeName(s"$$$$retier$$$name")

  def retierName(name: TermName) = TermName(s"$$$$retier$$${name.toString}")

  def retierName(name: TypeName) = TypeName(s"$$$$retier$$${name.toString}")

  implicit class NameOps(val name: Name) {
    def isRetierName = name.toString startsWith "$$retier$"
  }


  private case object RetierSynthetic

  def markRetierSynthetic(tree: Tree, pos: Position): tree.type =
    markRetierSynthetic(tree, pos, true)

  def markRetierSynthetic(tree: Tree, pos: Position, recursive: Boolean)
  : tree.type = {
    if (recursive)
      tree foreach { tree =>
        if (tree.pos == NoPosition)
          internal setPos (tree, pos)
      }
    else if (tree.pos == NoPosition)
      internal setPos (tree, pos)

    markRetierSynthetic(tree)
  }

  def markRetierSynthetic(tree: Tree): tree.type =
    markRetierSynthetic(tree, true)

  def markRetierSynthetic(tree: Tree, recursive: Boolean): tree.type = {
    if (recursive)
      tree foreach { internal updateAttachment (_, RetierSynthetic) }
    else
      internal updateAttachment (tree, RetierSynthetic)
    tree
  }

  def unmarkRetierSynthetic(tree: Tree): tree.type = {
    tree foreach { internal removeAttachment[RetierSynthetic.type] _ }
    tree
  }

  implicit class TreeOps(val tree: Tree) {
    def isRetierSynthetic: Boolean =
      (internal attachments tree).get[RetierSynthetic.type].nonEmpty

    def typeTree: Tree = typeTree(false)
    def typeTree(abortOnFailure: Boolean): Tree = tree match {
      case tree: TypeTree if tree.original != null =>
        internal setType (tree.original, tree.tpe)

      case _ if tree.tpe != null =>
        (typer createTypeTree tree.tpe) match {
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
        case _ => tree.tpe.typeArgs map TypeTree
      }
      (args zip tree.tpe.typeArgs) map { case (tree, tpe) =>
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
        (typer createTypeTree peerType) match {
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
