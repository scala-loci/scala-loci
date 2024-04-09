package loci
package utility

import scala.reflect.macros.Universe
import scala.util.control.NonFatal

object noReporting {
  def apply[T](universe: Universe, default: T)(body: => T) = {
    val reset = try {
      val reporterClass = Class.forName("scala.tools.nsc.reporters.Reporter")
      val filteringReporterClass = Class.forName("scala.tools.nsc.reporters.FilteringReporter")
      val noReporterClass = Class.forName("scala.tools.nsc.reporters.NoReporter")
      val settingsClass = Class.forName("scala.tools.nsc.Settings")

      val getRepoter = universe.getClass.getMethod("reporter")
      val setRepoter = universe.getClass.getMethod("reporter_$eq", reporterClass)
      val getSettings = filteringReporterClass.getMethod("settings")

      val reporter = getRepoter.invoke(universe)
      val settings = getSettings.invoke(reporter)
      val noReporter = noReporterClass.getConstructor(settingsClass).newInstance(settings)

      setRepoter.invoke(universe, noReporter)

      Some(setRepoter -> reporter)
    }
    catch {
      case NonFatal(_) =>
        None
    }

    reset.fold(default) { case (setRepoter, reporter) =>
      try body
      finally {
        try setRepoter.invoke(universe, reporter)
        catch { case NonFatal(_) => }
      }
    }
  }
}
