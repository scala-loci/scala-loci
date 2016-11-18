// support for DSL Paradise compiler plugin
// for boilerplate-free context injection

package object dslparadise {
  type `implicit =>`[-T, +R] = T => R
  type `implicit import =>`[-T, +R] = T => R
  type `import =>`[-T, +R] = T => R
  type `import`[T, I] = T

  type `argument name`[T <: _ => _, N] = T
}
