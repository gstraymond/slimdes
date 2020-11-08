package stuff

import stuff.block.Block.{H1, H2}
import stuff.block.{Block, Inline}
import stuff.presentation.Error.{NoH1Found, TooManyH1Found}

package object presentation {

  case class Presentation(title: List[Inline], slides: List[Slide])

  case class Slide(title: List[Inline], body: List[Block])

  sealed trait Error

  object Error {

    case object NoH1Found extends Error

    case object TooManyH1Found extends Error

  }

  def build(blocks: List[Block]): Either[Error, Presentation] = {
    val (h1s, others) = blocks.partition {
      case _: H1 => true
      case _ => false
    }

    h1s match {
      case Nil => Left(NoH1Found)
      case List(h1) => Right(buildPresentation(h1.asInstanceOf[H1], others))
      case _ => Left(TooManyH1Found)
    }
  }

  private def buildPresentation(h1: H1, blocks: List[Block]): Presentation = {
    val (slides, slide) = blocks.foldLeft(List.empty[Slide] -> Slide(Nil, Nil)) {
      case ((slides, slide), H2(inlines)) => (slides :+ slide) -> Slide(inlines, Nil)
      case ((slides, slide), block) => slides -> slide.copy(body = slide.body :+ block)
    }
    Presentation(
      h1.inlines,
      (slides :+ slide).filter(s => s.title.nonEmpty || s.body.nonEmpty)
    )
  }
}
