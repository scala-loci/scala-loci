package loci
package utility

import scala.collection.mutable
import scala.quoted.*
import scala.util.control.NonFatal

object noMacroCheck:
  def apply[T](body: => T)(using Quotes): T =
    apply(body)(body)

  def apply[T](body: => T)(default: => T)(using Quotes): T =
    val reset = try
      val context = quotes.getClass.getMethod("ctx").invoke(quotes)
      val settings = context.getClass.getMethod("settings").invoke(context)
      val XcheckMacros = settings.getClass.getMethod("XcheckMacros").invoke(settings)
      val idx = XcheckMacros.getClass.getMethod("idx").invoke(XcheckMacros)
      val settingsState = context.getClass.getMethod("settingsState").invoke(context)
      val valuesField = settingsState.getClass.getDeclaredField("values")

      valuesField.setAccessible(true)
      val values = valuesField.get(settingsState)

      (values, idx) match
        case (values: mutable.ArrayBuffer[Any @unchecked], idx: Int) =>
          values(idx) match
            case value: Boolean =>
              values(idx) = false
              Some(values, idx, value)
            case _ =>
              None
        case _ =>
          None

    catch
      case NonFatal(_) =>
        None

    reset.fold(default) { (values, idx, value) =>
      try
        body
      finally
        try values(idx) = value
        catch
          case NonFatal(_) =>
    }
end noMacroCheck
