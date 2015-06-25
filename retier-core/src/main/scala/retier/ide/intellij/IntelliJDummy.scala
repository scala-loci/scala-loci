package retier
package ide.intellij

@annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
trait IntelliJDummy

@annotation.compileTimeOnly("Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
object IntelliJDummy {
  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  implicit def materializeIntelliJDummy: IntelliJDummy = macro impl

  def impl(c: Context): c.Expr[IntelliJDummy] = {
    import c.universe._
    c.error(c.enclosingPosition, "Used to guide IntelliJ IDEA Scala Plugin type inference. Do not use directly.")
    c.Expr[IntelliJDummy](q"???")
  }
}
