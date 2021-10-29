package loci
package language
package impl
package components

import loci.valueref.PeerValueCache
import loci.valueref.PeerValueMapCache
import retypecheck._

import java.util.UUID
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.reflect.macros.blackbox

object Commons extends Component.Factory[Commons] {
  def apply[C <: blackbox.Context](engine: Engine[C]) = new Commons(engine)
  def asInstance[C <: blackbox.Context] = { case c: Commons[C] => c }
}

class Commons[C <: blackbox.Context](val engine: Engine[C]) extends Component[C] {
  val phases = Seq.empty

  import engine.c.universe._

  val retyper = engine.c.retyper

  val logging = Logging(engine.c)

  def expandMultitierModule(tree: ImplDef, name: Option[(String, TermName)]): ImplDef = {
    val result = engine.run(tree, name)
    val assembly = result.engine.require(Assembly)
    result.records collectFirst { case assembly.Assembly(tree) => tree } getOrElse tree
  }

  object names {
    val tuple = "scala.Tuple"
    val root = termNames.ROOTPKG
    val tie = TypeName("Tie")
    val base = TypeName("Base")
    val intermediate = TypeName("Intermediate")
    val result = TypeName("Result")
    val proxy = TypeName("Proxy")
    val transmittables = TypeName("Transmittables")
    val running = TermName("running")
    val terminate = TermName("terminate")
    val recursive = TermName("recursive")
    val placedValues = TermName(NameTransformer encode "<placed values>")
    def placedValues(symbol: Symbol) =
      TypeName(NameTransformer encode s"<placed values of ${uniqueRealisticName(symbol)}>")
  }

  object symbols {
    val on = symbolOf[_ on _]
    val per = symbolOf[_ per _]
    val from = symbolOf[_ from _]
    val fromSingle = symbolOf[_ fromSingle _]
    val fromMultiple = symbolOf[_ fromMultiple _]
    val local = symbolOf[Local[_]]
    val placedValue = symbolOf[PlacedValue[_, _]]
    val On = symbolOf[Placement.On[_]]
    val Placed = symbolOf[Placement.Placed]
    val Select = symbolOf[Placement.Select[Placement.Run]]
    val SelectAny = symbolOf[Placement.SelectAny[Placement.Run]]
    val Narrow = symbolOf[Placement.Narrow]
    val Call = symbolOf[Placement.Call[_, PlacedValue]]
    val Block = symbolOf[Placement.Block[_, PlacedValue]]
    val Capture = symbolOf[Placement.Capture[_, PlacedValue]]
    val placement = symbolOf[Placement.type]
    val serializable = symbolOf[transmitter.Serializable.type]
    val transmittableDummy = symbolOf[transmitter.transmittable.TransmittableDummy]
    val remoteSelection = symbolOf[RemoteSelection.type]
    val placedValues = engine.c.mirror.staticModule("_root_.loci.runtime.PlacedValues")
    val cast = typeOf[runtime.Remote.type] member TermName("cast")
    val and = typeOf[Placed[_, _]] member TermName("and")
    val to = typeOf[Placed[_, _]] member TermName("to")
    val froms = (typeOf[Placed[_, _]] member TermName("from")).alternatives
    val lifts = (symbolOf[Placed[_, _]].companion.info member TermName("lift")).alternatives
    val multitier = symbolOf[loci.multitier.type]
  }

  object types {
    val function = typeOf[_ => _]
    val biFunction = typeOf[(_, _) => _]
    val stringList = typeOf[List[String]]
    val future = typeOf[concurrent.Future[_]]
    val unitFuture = typeOf[concurrent.Future[Unit]]
    val nothingFuture = typeOf[concurrent.Future[Nothing]]
    val on = typeOf[_ on _]
    val per = typeOf[_ per _]
    val from = typeOf[_ from _]
    val fromSingle = typeOf[_ fromSingle _]
    val fromMultiple = typeOf[_ fromMultiple _]
    val multiple = typeOf[Multiple[_]]
    val optional = typeOf[Optional[_]]
    val single = typeOf[Single[_]]
    val peer = typeOf[peer]
    val peergroup = typeOf[peergroup]
    val peerMarker = typeOf[Peer]
    val remote = typeOf[Remote[_]]
    val remoteSeq = typeOf[Seq[Remote[_]]]
    val placedValue = typeOf[PlacedValue[_, _]]
    val subjective = typeOf[Placed.Subjective[_, _]]
    val singleSelection = typeOf[Placed.Selected.Single[_]]
    val multipleSelection = typeOf[Placed.Selected.Multiple[_]]
    val messageBuffer = typeOf[MessageBuffer]
    val system = typeOf[runtime.System]
    val signature = typeOf[runtime.Value.Signature]
    val valueReference = typeOf[runtime.Value.Reference]
    val abstractValue = typeOf[runtime.AbstractValue]
    val multitierStub = typeOf[runtime.MultitierStub]
    val multitierModule = typeOf[runtime.MultitierModule]
    val marshallableValue = typeOf[runtime.MarshallableValue[_, _, _, _]]
    val marshallableInfo = typeOf[runtime.MarshallableInfo]
    val placedRuntimeValue = typeOf[runtime.PlacedValue[_, _, _, _]]
    val placedRuntimeValueInfo = typeOf[runtime.PlacedValueInfo]
    val serializable = typeOf[transmitter.Serializable[_]]
    val transmittable = typeOf[transmitter.transmittable.Transmittable.Any[_, _, _]]
    val resolution = typeOf[transmitter.transmittable.Transmittable.Resolution[_, _, _, _, _]]
    val gatewayConnection = typeOf[runtime.GatewayConnection[_, _]]
    val remoteRequest = typeOf[runtime.RemoteRequest[_, _, _, _, _, _]]
    val moduleSignature = typeOf[runtime.Module.Signature]
    val peerSignature = typeOf[runtime.Peer.Signature]
    val tieSignature = typeOf[Map[runtime.Peer.Signature, runtime.Peer.Tie]]
    val remoteAccessException = typeOf[transmitter.RemoteAccessException]
    val connection = typeOf[transmitter.Connection[_, _]]
    val transmission = typeOf[transmitter.Transmission[_, _, _, _, _]]
    val remoteGateway = typeOf[transmitter.RemoteGateway]
    val remoteAccessor = typeOf[transmitter.RemoteAccessor]
    val delegates = typeOf[transmitter.transmittable.Transmittables.Delegates[_ ]]
    val message = typeOf[transmitter.transmittable.Transmittables.Message[_]]
    val none = typeOf[transmitter.transmittable.Transmittables.None]
    val compileTimeOnly = typeOf[annotation.compileTimeOnly]
    val placedValues = symbols.placedValues.companion.asType.toType
    val transmitterMultiple = typeOf[language.Tie.Multiple]
    val defaultMultipleGateway = typeOf[language.Gateway.DefaultMultipleGateway[_]]
    val selfReference = typeOf[SelfReference[_]]
    val futureWrappingSelfReferenceDummyRequest = typeOf[runtime.FutureWrappingSelfReferenceDummyRequest[_, _, _, _, _]]
    val identicalSelfReferenceDummyRequest = typeOf[runtime.IdenticalSelfReferenceDummyRequest[_, _, _, _, _]]
    val blockingRemoteAccessor = typeOf[transmitter.Blocking]
    val basicBlockingSingleAccessor = typeOf[PlacedValue.BasicBlockingSingleAccessor[_, _, _, _]]
    val basicSingleAccessor = typeOf[PlacedValue.BasicSingleAccessor[_, _, _, _]]
    val context = typeOf[Placement.Context[_]]
    val nonInstantiable = typeOf[NonInstantiable]
    val union = typeOf[_ | _]
    val uniquePeerId = typeOf[UUID]
    val valueRefCreator = typeOf[valueref.ValueRefCreator[_, _]]
    val peerValueCache = typeOf[PeerValueCache[_]]
    val peerValueMapCache = typeOf[PeerValueMapCache[_]]
  }

  object trees {
    val implicitly = q"${names.root}.scala.Predef.implicitly"
    val `try` = q"${names.root}.scala.util.Try"
    val nil = q"${names.root}.scala.collection.immutable.Nil"
    val map = q"${names.root}.scala.collection.immutable.Map.apply"
    val empty = q"${names.root}.loci.MessageBuffer.empty"
    val reference = q"${names.root}.loci.Remote.reference"
    val remoteGateway = q"${names.root}.loci.runtime.GatewayValue"
    val remoteValue = q"${names.root}.loci.runtime.RemoteValue"
    val moduleSignature = q"${names.root}.loci.runtime.Module.Signature.apply"
    val peerSignature = q"${names.root}.loci.runtime.Peer.Signature.apply"
    val valueSignature = q"${names.root}.loci.runtime.Value.Signature.apply"
    val multiple = q"${names.root}.loci.runtime.Peer.Tie.Multiple"
    val optional = q"${names.root}.loci.runtime.Peer.Tie.Optional"
    val single = q"${names.root}.loci.runtime.Peer.Tie.Single"
    val illegalSubjectiveAccess = q"${names.root}.loci.transmitter.RemoteAccessException.IllegalSubjectiveAccess"
    val serializable = q"${names.root}.loci.transmitter.Serializable.apply"
    val marshallable = q"${names.root}.loci.transmitter.Marshallable.marshallable"
    val nothingMarshallable = q"${names.root}.loci.transmitter.Marshallable.nothing"
    val unitMarshallable = q"${names.root}.loci.transmitter.Marshallable.unit"
    val delegating = q"${names.root}.loci.transmitter.transmittable.ContextBuilder.delegating"
    val messaging = q"${names.root}.loci.transmitter.transmittable.ContextBuilder.messaging"
    val none = q"${names.root}.loci.transmitter.transmittable.ContextBuilder.none"
    val delegate = q"${names.root}.loci.transmitter.transmittable.ContextBuilders.delegate"
    val list = q"${names.root}.loci.transmitter.transmittable.ContextBuilders.list"
    val basicBlockingSingleAccessor = q"${names.root}.loci.language.PlacedValue.BasicBlockingSingleAccessor"
    val remote = q"${names.root}.loci.`package`.remote.apply"
    val futureSuccessful = q"${names.root}.scala.concurrent.Future.successful"
    val generateUniquePeerId = q"${names.root}.loci.valueref.UniquePeerId.generate()"
  }

  def createTypeTree(tpe: Type, pos: Position): Tree = {
    def containsTypeTree(tree: Tree) = tree exists {
      case _: TypeTree => true
      case _ => false
    }

    val tree = retyper.createTypeTree(tpe, pos)
    if (containsTypeTree(tree)) {
      val underlyingTree = retyper.createTypeTree(tpe.underlying, pos)
      if (containsTypeTree(underlyingTree))
        tree
      else
        underlyingTree
    }
    else
      tree
  }

  def createTypeTree(tree: Tree): Tree =
    tree match {
      case tree: TypeTree =>
        if (tree.original != null)
          tree.original
        else
          createTypeTree(tree.tpe, tree.pos)
      case _ =>
        tree
    }

  def uniqueRealisticTermName(symbol: Symbol): TermName =
    TermName(uniqueName(symbol, NameTransformer encode ".", NameTransformer encode "#", ""))

  def uniqueRealisticName(symbol: Symbol): String = uniqueName(symbol, ".", "#", "")

  def uniqueName(symbol: Symbol, name: String = ""): String = uniqueName(symbol, "$", "$_$", name)

  private def uniqueName(symbol: Symbol, selection: String, projection: String, name: String): String = {
    val symbolOwner = symbol.owner

    val symbolName = {
      val symbolName = symbol.name.toString
      if (symbolName startsWith "$loci$multitier$")
        (symbolOwner.info member TermName(symbolName.drop(16)) orElse symbol).name.toString
      else
        symbolName
    }

    if (symbolOwner != NoSymbol && (symbol.isSynthetic || ((symbolName startsWith "<") && (symbolName endsWith ">"))))
      uniqueName(symbolOwner, selection, projection, name)
    else {
      val prefix =
        if (symbolOwner == NoSymbol || symbolOwner == engine.c.mirror.RootClass)
          symbolName
        else
          uniqueName(symbolOwner, selection, projection, symbolName)

      if (prefix.isEmpty)
        name
      else if (name.nonEmpty) {
        val separator = if (symbol.isType && !symbol.isModuleClass) projection else selection

        val suffix = (
          if (name endsWith termNames.LOCAL_SUFFIX_STRING)
            name.dropRight(termNames.LOCAL_SUFFIX_STRING.length)
          else
            name
        ).stripPrefix("$") // avoid "$$" in name (https://github.com/sbt/zinc/issues/531)

        s"$prefix$separator$suffix"
      }
      else
        prefix
    }
  }

  implicit class ListOps[T](list: List[T]) {
    def process(f: PartialFunction[T, T]): List[T] =
      list map { v => f.applyOrElse(v, identity[T]) }
    def flatProcess(f: PartialFunction[T, compatibility.IterableOnce[T]]): List[T] =
      list flatMap { v => f.applyOrElse(v, compatibility.Iterable(_: T)) }
  }

  implicit class TypeOps(tpe: Type) {
    def =:!=(other: Type): Boolean = !(tpe =:= other)

    def <:!<(other: Type): Boolean = !(tpe <:< other)

    def real_<:!<(other: Type): Boolean = !(tpe real_<:< other)

    def real_<:<(other: Type): Boolean = tpe != null && tpe <:< other &&
      tpe <:!< definitions.NothingTpe && tpe <:!< definitions.NullTpe

    def underlying: Type =
      if (tpe ne tpe.dealias)
        tpe.dealias.underlying
      else if (tpe ne tpe.widen)
        tpe.widen.underlying
      else
        tpe

    def mapArgs(f: List[Type] => List[Type]): Type = tpe match {
      case ExistentialType(quantified, TypeRef(pre, sym, args)) =>
        val newArgs = f(args)
        val newQuantified = quantified filter { symbol =>
          newArgs exists { _ contains symbol }
        }
        val newUnderlying = internal.typeRef(pre, sym, newArgs)
        if (newQuantified.nonEmpty)
          internal.existentialType(newQuantified, newUnderlying)
        else
          newUnderlying
      case TypeRef(pre, sym, args) =>
        internal.typeRef(pre, sym, f(args))
      case _ =>
        tpe
    }

    def asSeenFrom(symbol: ClassSymbol): Type = {
      val bases = symbol.selfType.baseClasses.toSet
      val thisType = internal.thisType(symbol)
      val symbols = mutable.ListBuffer.empty[Symbol]

      tpe foreach {
        case TypeRef(_, sym, _) => symbols += sym.owner
        case ThisType(sym) => symbols += sym
        case SingleType(_, sym) if sym.isModule => symbols += sym.asModule.moduleClass
        case SingleType(_, sym) if sym.isModuleClass => symbols += sym
        case _ =>
      }

      symbols.foldLeft(tpe) { (tpe, symbol) =>
        if (bases contains symbol)
          tpe.asSeenFrom(thisType, symbol)
        else
          tpe
      }
    }
  }

  implicit class SymbolOps(symbol: Symbol) {
    def allAnnotations: List[Annotation] =
      if (symbol.isMethod && symbol.asMethod.isAccessor)
        symbol.annotations ++ symbol.asMethod.accessed.annotations
      else
        symbol.annotations

    def ancestors: List[Symbol] =
      if (symbol.owner != NoSymbol)
        symbol.owner :: symbol.owner.ancestors
      else
        List.empty

    def nameInEnclosing: String = {
      val name = symbol.fullName
      val tpe =
        if (symbol.isType)
          symbol.asType.toType
        else
          NoType

      (Seq(
          definitions.UnitTpe, definitions.ByteTpe, definitions.ShortTpe, definitions.CharTpe,
          definitions.IntTpe, definitions.LongTpe, definitions.FloatTpe, definitions.DoubleTpe,
          definitions.BooleanTpe, definitions.AnyTpe, definitions.AnyValTpe, definitions.AnyRefTpe,
          definitions.ObjectTpe, definitions.NothingTpe, definitions.NullTpe)
        collectFirst {
          case standardTpe if tpe =:= standardTpe => standardTpe.toString
        }
        getOrElse {
          val index = if (name startsWith "scala.") -1 else name lastIndexOf "."
          if (index > 0)
            s"${name.substring(index + 1)} in ${name.substring(0, index)}"
          else
            name
        })
    }
  }

  implicit class PositionOps(pos: Position) {
    def orElse(other: Position): Position = if (pos == NoPosition) other else pos
  }

  implicit class ModifiersOps(mods: Modifiers) {
    def withFlags(flags: FlagSet): Modifiers =
      Modifiers(mods.flags | flags, mods.privateWithin, mods.annotations)

    def withoutFlags(flags: FlagSet): Modifiers = {
      val reducedFlags =
        Seq(Flag.ABSOVERRIDE, Flag.ABSTRACT, Flag.ARTIFACT, Flag.BYNAMEPARAM,
            Flag.CASE, Flag.CASEACCESSOR, Flag.CONTRAVARIANT, Flag.COVARIANT,
            Flag.DEFAULTINIT, Flag.DEFAULTPARAM, Flag.DEFERRED, Flag.FINAL,
            Flag.IMPLICIT, Flag.INTERFACE, Flag.LAZY, Flag.LOCAL, Flag.MACRO,
            Flag.MUTABLE, Flag.OVERRIDE, Flag.PARAM, Flag.PARAMACCESSOR,
            Flag.PRESUPER, Flag.PRIVATE, Flag.PROTECTED, Flag.SEALED,
            Flag.STABLE, Flag.SYNTHETIC, Flag.TRAIT)
          .foldLeft(NoFlags) { (flagAcc, flag) =>
            if ((flags != (flags | flag)) && (mods hasFlag flag)) flagAcc | flag else flagAcc
          }
      Modifiers(reducedFlags, mods.privateWithin, mods.annotations)
    }
  }

  implicit class TreeOps(tree: Tree) {
    def original: Tree = tree match {
      case tree: TypeTree => tree.original
      case tree => tree
    }

    def fullyExpanded: Tree =
      typeTreeExpander transform tree
  }

  implicit class ImplDefOps(tree: ImplDef) {
    def map(f: (Modifiers, List[Tree], ValDef, List[Tree]) =>
               (Modifiers, List[Tree], ValDef, List[Tree])): ImplDef = (tree: @unchecked) match {
      case ClassDef(mods, tpname, tparams, impl @ Template(parents, self, body)) =>
        val (modsNew, parentsNew, selfNew, bodyNew) = f(mods, parents, self, body)
        treeCopy.ClassDef(tree, modsNew, tpname, tparams,
          treeCopy.Template(impl, parentsNew, selfNew, bodyNew))

      case ModuleDef(mods, tpname, impl @ Template(parents, self, body)) =>
        val (modsNew, parentsNew, selfNew, bodyNew) = f(mods, parents, self, body)
        treeCopy.ModuleDef(tree, modsNew, tpname,
          treeCopy.Template(impl, parentsNew, selfNew, bodyNew))
    }
  }

  implicit class ValOrDefDefOps(tree: ValOrDefDef) {
    def map(f: (Modifiers, TermName, Tree, Tree) =>
               (Modifiers, TermName, Tree, Tree)): ValOrDefDef = (tree: @unchecked) match {
      case ValDef(mods, name, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        treeCopy.ValDef(tree, modsNew, nameNew, tptNew, rhsNew)

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val (modsNew, nameNew, tptNew, rhsNew) = f(mods, name, tpt, rhs)
        treeCopy.DefDef(tree, modsNew, nameNew, tparams, vparamss, tptNew, rhsNew)
    }
  }

  private object typeTreeExpander extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case tree: TypeTree if tree.tpe != null =>
        retyper.createTypeTree(tree.tpe map { _.dealias }, tree.pos) match {
          case tree: TypeTree => tree
          case tree => transform(tree)
        }
      case _ =>
        super.transform(tree)
    }
  }
}
