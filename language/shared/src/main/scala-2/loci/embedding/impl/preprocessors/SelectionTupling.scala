package loci
package embedding
package impl
package preprocessors

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

object SelectionTupling extends Preprocessor.Factory[SelectionTupling] {
  def apply[C <: blackbox.Context](c: C) = new SelectionTupling(c)
}

class SelectionTupling[C <: blackbox.Context](val c: C) extends Preprocessor[C] {
  import c.universe._

  def process(tree: Tree): Tree = {
    val multiargInfixDetection: Either[ClassTag[_], Array[Char]] =
      try Left(ClassTag(Class.forName("scala.reflect.internal.StdAttachments$MultiargInfixAttachment$")))
      catch { case NonFatal(_) => Right(c.enclosingPosition.source.content) }

    object transformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(apply @ Apply(select @ Select(qualifier, TermName("from")), selection), name)
            if selection.size >= 2 && selection.size <= 22 && isStable(qualifier) =>

          val infixArguments = multiargInfixDetection match {
            case Left(classTag) =>
              internal.attachments(apply) contains classTag
            case Right(content) =>
              isInfix(content, select.pos.point)
          }

          val tupledTree =
            if (infixArguments) {
              multiargInfixDetection.left foreach { classTag =>
                internal.removeAttachment(apply)(classTag)
              }

              val tuple = atPos(select.pos) {
                val tuple = TermName(s"Tuple${selection.size}")
                q"${termNames.ROOTPKG}.scala.$tuple(..$selection)"
              }

              Some(treeCopy.Select(tree,
                treeCopy.Apply(apply,
                  treeCopy.Select(select, qualifier, TermName("from")),
                  List(tuple)),
                name))
            }
            else
              None

          super.transform(tupledTree getOrElse tree)

        case _ =>
          super.transform(tree)
      }
    }

    def isStable(tree: Tree): Boolean = tree match {
      case Select(qualifier, _) => isStable(qualifier)
      case Ident(_) | This(_) | Super(_, _) => true
      case _ => false
    }

    def isInfix(content: Array[Char], pos: Int): Boolean = {
      var current = pos
      var found = ' '
      var comment = 0
      var potentialComment = -1

      while (current > 0)
        if (content(current - 1) == '*' && content(current) == '/') {
          if (potentialComment >= 0)
            potentialComment += 1
          comment += 1
          current -= 2
        }
        else if (content(current - 1) == '/' && content(current) == '*') {
          if (potentialComment >= 0)
            potentialComment -= 1
          comment -= 1
          current -= 2
        }
        else if (content(current) == '\n' || content(current) == '\r') {
          if (comment == 0 && found != ' ')
            current = 0

          potentialComment = 0
          current -= 1
        }
        else if (content(current - 1) == '/' && content(current) == '/') {
          if (potentialComment >= 0) {
            comment -= potentialComment
            potentialComment = 0
          }
          found = ' '
          current -= 2
        }
        else if (comment > 0)
          current -= 1
        else if (!Character.isWhitespace(content(current)) && found == ' ') {
          found = content(current)
          current -= 1

          if (potentialComment == -1)
            current = 0
        }
        else
          current -= 1

      found != '.'
    }

    if ((c inferImplicitValue typeOf[feature.ManualSelectionTupling]).isEmpty)
      transformer transform tree
    else
      tree
  }
}
