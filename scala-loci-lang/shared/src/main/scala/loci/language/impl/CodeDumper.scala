package loci.language.impl

import loci.language.impl.components.Assembly
import loci.language.impl.components.Initialization
import loci.language.impl.components.Values
import org.json4s.DateFormat
import org.json4s.Extraction
import org.json4s.Formats
import org.json4s.JArray
import org.json4s.JNull
import org.json4s.JObject
import org.json4s.JString
import org.json4s.JValue
import org.json4s.ShortTypeHints
import org.json4s.TypeHints
import org.json4s.native.Serialization.writePretty

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import scala.reflect.macros.blackbox

object CodeDumper {
  def apply(c: blackbox.Context) = new CodeDumper(c)
}

class CodeDumper(c: blackbox.Context) {

  private lazy val path: Option[String] = {
    c.settings.collectFirst {
      case s"loci.macro.codepath_$path" => path
    }
  }

  def isEnabled: Boolean = {
    path.isDefined
  }

  def dump(code: String, moduleName: String): Unit = {
    Files.write(
      Paths.get(
        path.getOrElse(throw new RuntimeException("Path to dump code was undefined")),
        s"$moduleName.scala"
      ),
      code.getBytes(StandardCharsets.UTF_8)
    )
  }

  def recordsFormats(engine: Engine[_ <: blackbox.Context]): Formats = new Formats {
    private val initialization = engine.require(Initialization)
    import initialization.Initialized
    private val values = engine.require(Values)
    import values.PlacedValueDef
    import values.PlacedValuePeerImpl
    import values.ModuleValue
    private val assembly = engine.require(Assembly)
    import assembly.Assembly

    override def dateFormat: DateFormat = throw new NotImplementedError

    override val strictOptionParsing: Boolean = false

    override val typeHints: TypeHints = new ShortTypeHints(
      List(
        classOf[Initialized],
        classOf[PlacedValueDef],
        classOf[PlacedValuePeerImpl],
        classOf[ModuleValue],
        classOf[Assembly]
      )
    ) {
      override def serialize: PartialFunction[Any, JObject] = {
        case initialization: Initialized =>
          JObject(
            "tree" -> JString(initialization.tree.toString.linesWithSeparators.mkString)
          )
        case placedValueDef: PlacedValueDef =>
          JObject(
            "symbol" -> JString(placedValueDef.symbol.fullName),
            "tree" -> JString(placedValueDef.tree.toString.linesWithSeparators.mkString),
            "peer" -> placedValueDef.peer.map(_.fullName).map(JString).getOrElse(JNull),
            "modality" -> JString(placedValueDef.modality.getClass.getSimpleName)
          )
        case placedValuePeerImpl: PlacedValuePeerImpl =>
          JObject(
            "symbol" -> JString(placedValuePeerImpl.symbol.fullName),
            "tree" -> JString(placedValuePeerImpl.tree.toString.linesWithSeparators.mkString),
            "peer" -> JString(placedValuePeerImpl.peer.fullName),
            "modality" -> JString(placedValuePeerImpl.modality.getClass.getSimpleName)
          )
        case moduleValue: ModuleValue =>
          JObject(
            "symbol" -> JString(moduleValue.symbol.fullName),
            "tree" -> JString(moduleValue.tree.toString.linesWithSeparators.mkString)
          )
        case assembly: Assembly =>
          JObject(
            "tree" -> JString(assembly.tree.toString.linesWithSeparators.mkString)
          )
      }
    }
  }

  def toJValue(a: Any)(implicit formats: Formats): JValue = Extraction.decompose(a)

  private def toJson(phaseResults: Seq[PhaseResult])(implicit formats: Formats): String = {
    val resultJsons = phaseResults.map(r => JObject("phase" -> JString(r.phase), "records" -> r.json))
    writePretty(JArray(resultJsons.toList))
  }

  def dump(results: Seq[PhaseResult], moduleName: String)(implicit format: Formats): Unit = {
    Files.write(
      Paths.get(
        path.getOrElse(throw new RuntimeException("Path to dump code was undefined")),
        s"$moduleName.json"
      ),
      toJson(results).getBytes(StandardCharsets.UTF_8)
    )
  }

}

case class PhaseResult(
  phase: String,
  records: List[Any],
  json: JValue
)
