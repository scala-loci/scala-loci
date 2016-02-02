// support for DSL Paradise compiler plugin
// for boilerplate-free context injection

package object dslparadise {
  type `implicit =>`[-T, +R] = T => R

  type `import._ =>`[-T, +R] = T => R

  type `import._`[+T, I] = T
}
