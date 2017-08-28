package loci

package object impl {
  private[impl] def className[T](v: T): String =
    if (v == null) "null" else v.getClass.getName
}
