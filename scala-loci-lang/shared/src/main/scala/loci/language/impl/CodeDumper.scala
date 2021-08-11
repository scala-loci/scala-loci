package loci.language.impl

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

}
