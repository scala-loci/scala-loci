import sbt._
import sbt.Keys._
import sbtcrossproject.{CrossClasspathDependency => Dependency, CrossProject => Project}
import LociUtil.autoImport._

object lociProject extends LociProjectBuilder(includeScalaVersion = _ => true) {
  def apply(project: Project) = project
  def apply(project: sbt.Project) = project
  def scala2only = new LociProjectBuilder(includeScalaVersion = `is 2.x`)
}

object Projects {
  def apply(dependencies: Dependency*) = dependencies.toSeq
}

class LociProjectBuilder(includeScalaVersion: String => Boolean) {
  def apply(name: String, project: Project): Project =
    apply(name, name, project, Seq.empty)

  def apply(name: String, file: String, project: Project): Project =
    apply(name, file, project, Seq.empty)

  def apply(name: String, project: Project, dependsOn: Dependency): Project =
    apply(name, name, project, Seq(dependsOn))

  def apply(name: String, file: String, project: Project, dependsOn: Dependency): Project =
    apply(name, file, project, Seq(dependsOn))

  def apply(name: String, project: Project, dependsOn: Seq[Dependency]): Project =
    apply(name, name, project, dependsOn)

  def apply(name: String, file: String, project: Project, dependsOn: Seq[Dependency]): Project = {
    val normalizedName = sbt.Project.normalizeModuleID(file)

    val lociProject = project in sbt.file(normalizedName) settings (
      Keys.name := s"ScalaLoci $name",
      Keys.normalizedName := s"scala-loci-$normalizedName",

      Keys.libraryDependencies ~= {
        _ map { module =>
          if (module.configurations contains "test") module.withConfigurations(Some("test-internal")) else module
        }
      },

      compile / skip := (compile / skip).value || !includeScalaVersion(scalaVersion.value),
      publish / skip := (publish / skip).value || !includeScalaVersion(scalaVersion.value),
    )

    if (dependsOn.nonEmpty)
      lociProject dependsOn (dependsOn map { dependency =>
        if (dependency.configuration.nonEmpty)
          dependency
        else
          new Dependency(dependency.project, Some("compile->compile;test-internal->test-internal"))
      }: _*)
    else
      lociProject
  }
}
