import sbt._
import sbt.Keys._

object Util extends AutoPlugin {
  object autoImport {
    val ScalaJSCrossVersion = org.scalajs.sbtplugin.ScalaJSCrossVersion

    val RuntimeInternal = sbt.librarymanagement.Configurations.RuntimeInternal
    val TestInternal = sbt.librarymanagement.Configurations.TestInternal
    val IntegrationTestInternal = sbt.librarymanagement.Configurations.IntegrationTestInternal
    val CompileInternal = sbt.librarymanagement.Configurations.CompileInternal


    def `is 2.12+`(scalaVersion: String) =
      CrossVersion.partialVersion(scalaVersion) exists { case (m, n) => m >= 3 || m == 2 && n >= 12 }

    def `is 2.13+`(scalaVersion: String) =
      CrossVersion.partialVersion(scalaVersion) exists { case (m, n) => m >= 3 || m == 2 && n >= 13 }

    def `is 3+`(scalaVersion: String) =
      CrossVersion.partialVersion(scalaVersion) exists { case (m, _) => m >= 3 }


    def rebaseFile(file: File, oldBase: File, newBase: File) =
      IO.relativize(oldBase, file) map { newBase / _ }


    val copyCompiledFiles = taskKey[Unit]("Copies the compiled files from one project to another.")

    def copyCompiledFilesFrom(project: Project) = {
      def copyCompiledFilesFrom(project: Project, config: Configuration) = Seq(
        config / copyCompiledFiles := IO.copyDirectory(
          (project / config / classDirectory).value,
          (config / classDirectory).value,
          overwrite = false, preserveLastModified = true, preserveExecutable = true),
        config / copyCompiledFiles :=
          ((config / copyCompiledFiles) dependsOn (project / config / compile)).value,
        config / compile :=
          ((config / compile) dependsOn (config / copyCompiledFiles)).value)

      copyCompiledFilesFrom(project, Compile) ++ copyCompiledFilesFrom(project, Test)
    }
  }
}
