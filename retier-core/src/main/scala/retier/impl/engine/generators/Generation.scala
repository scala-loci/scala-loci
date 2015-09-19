package retier
package impl
package engine.generators

import engine._
import scala.reflect.macros.blackbox.Context

trait Generation {
  val c: Context
  import c.universe._

  object types {
    val bottom = Seq(typeOf[Nothing], typeOf[Null])

    val retier = typeOf[_root_.retier.`package`.type]
    val peer = typeOf[Peer]

    val localOn = typeOf[_ localOn _]
    val sharedOn = typeOf[_ sharedOn _]
    val issued = typeOf[_ <-> _]
    val issuedControlled = typeOf[_ <=> _]

    val placing = typeOf[PlacingExpression[_]]
    val issuing = typeOf[IssuingExpression[_, _]]
    val overriding = typeOf[OverridingExpression[_]]

    val functionPlacing = Seq(typeOf[_ => _], typeOf[(_ => _) localOn _])
    val issuedPlacing = Seq(typeOf[_ <=> _], typeOf[(_ <=> _) localOn _])

    val transmissionProvider = typeOf[transmission.TransmissionProvider]

    val multiple = typeOf[Peer#Multiple[_]]
    val optional = typeOf[Peer#Optional[_]]
    val single = typeOf[Peer#Single[_]]

    val peerTypeTag = typeOf[PeerTypeTag[_]]
  }

  object symbols {
    val retier = types.retier.termSymbol

    val placedApply = types.placing member TermName("apply")
    val placedShared = types.placing member TermName("shared")
    val placedLocal = types.placing member TermName("local")
    val placedIssued = types.placing member TermName("issued")
    val placedIssuedApply = types.issuing member TermName("apply")
    val placedOverriding = types.overriding member TermName("overriding")

    val placed = Seq(placedApply, placedShared, placedLocal, placedIssuedApply)

    val transmitMultiple = types.retier member TermName("transmitMultiple")
    val transmitOptional = types.retier member TermName("transmitOptional")
    val transmitSingle = types.retier member TermName("transmitSingle")

    val transmit = Seq(transmitMultiple, transmitOptional, transmitSingle)

    val discardValue = types.retier member TermName("discardValue")
    val issueValue = types.retier member TermName("issueValue")
    val issueValueControlled = types.retier member TermName("issueValueControlled")

    val reduceCapture = types.retier member TermName("reduceCapture")
    val liftCapture = types.retier member TermName("liftCapture")

    val liftValueGlobally = types.retier member TermName("liftValueGlobally")
    val liftValueLocally = types.retier member TermName("liftValueLocally")
    val downcastValueGlobally = types.retier member TermName("downcastValueGlobally")
    val downcastValueLocally = types.retier member TermName("downcastValueLocally")

    val globalCasts = Seq(discardValue, issueValue, issueValueControlled,
      liftValueGlobally, downcastValueGlobally)
    val localCasts = Seq(reduceCapture, liftCapture, liftValueLocally,
      downcastValueLocally)
    val casts = globalCasts ++ localCasts
  }

  object names {
    val peerTypeTag = TermName("peerTypeTag")
    val peerType = TermName("peerType")
    val connection = TermName("Connection")
    val system = retierTermName("system")
    val implementation = retierTypeName("peer")
    val interface = retierTermName("peer")
    val dispatch = retierTermName("dispatch")
  }

  object trees {
    private[this] val root = termNames.ROOTPKG
    val Try = tq"$root.scala.util.Try"
    val String = tq"$root.scala.Predef.String"
    val implicitly = q"$root.scala.Predef.implicitly"
    val Success = q"$root.scala.util.Success"
    val Failure = q"$root.scala.util.Failure"
    val List = q"$root.scala.collection.immutable.List"
    val Map = q"$root.scala.collection.immutable.Map"
    val peerTypeOf = q"$root.retier.peerTypeOf"
    val AbstractionId = tq"$root.retier.transmission.AbstractionId"
    val AbstractionIdCreate = q"$root.retier.impl.AbstractionId.create"
    val AbstractionRef = tq"$root.retier.transmission.AbstractionRef"
    val Marshallable = tq"$root.retier.transmission.Marshallable"
    val UnitMarshallable = q"$root.retier.impl.UnitMarshallable"
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
    val IssuedValueCreate = q"$root.retier.impl.IssuedValue.create"
    val ControlledIssuedValueCreate = q"$root.retier.impl.ControlledIssuedValue.create"
    val System = tq"$root.retier.impl.System"
  }


  case class EnclosingContext(name: TypeName, bases: List[Tree])

  case class InputStatement(stat: Tree)

  case class PeerDefinition(tree: Tree, peerName: TypeName, peerType: Type,
    typeArgs: List[Tree], args: List[List[Tree]], parents: List[Tree],
    mods: Modifiers, stats: List[Tree], isClass: Boolean,
    companion: Option[Tree])

  case class PlacedStatement(tree: Tree, peerType: Type, exprType: Type,
    declTypeTree: Option[Tree], overridingDecl: Option[TermName], expr: Tree)

  case class NonPlacedStatement(tree: Tree)

  case class PlacedAbstraction(peerType: Type, interfaceDefinitions: List[Tree],
    dispatchClause: CaseDef)

  case class PeerConnectionMultiplicity(peerType: Type, connectedPeer: Tree,
    connectionMultiplicity: Tree)


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

  def markRetierSynthetic(tree: Tree, pos: Position = NoPosition): tree.type = {
    tree foreach { tree =>
      if (tree.pos == NoPosition)
        internal setPos (tree, pos)
    }
    markRetierSynthetic(tree)
  }

  def markRetierSynthetic(tree: Tree): tree.type = {
    tree foreach { internal updateAttachment (_, RetierSynthetic) }
    tree
  }

  def unmarkRetierSynthetic(tree: Tree): tree.type = {
    tree foreach { internal removeAttachment[RetierSynthetic.type] _ }
    tree
  }

  implicit class TreeOps(val tree: Tree) {
    def isRetierSynthetic: Boolean =
      (internal attachments tree).get[RetierSynthetic.type].nonEmpty
    def typeTree: Tree = tree match {
      case tree: TypeTree if tree.original != null => tree.original
      case _ if tree.tpe != null => typer createTypeTree tree.tpe
      case _ => tree
    }
  }


  implicit class TypeOps(tpe: Type) {
    def =:!=(that: Type): Boolean = !(tpe =:= that)
    def <:!<(that: Type): Boolean = !(tpe <:< that)
    def isGeneric: Boolean = tpe exists { _.typeSymbol.isParameter }
  }
}
