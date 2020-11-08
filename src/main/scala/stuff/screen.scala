package stuff

import stuff.block.Block.{ListItem, Par}
import stuff.block.{Block, Inline}
import stuff.presentation.Presentation

package object screen {

  case class Screen(prints: List[Print])

  case class Print(text: String, line: Int, column: Int, attribute: Option[Int])

  def build(presentation: Presentation,
            lines: Int,
            columns: Int,
            hasColors: Boolean): List[Screen] = {
    val title = toText(presentation.title)
    presentation.slides.zipWithIndex.map { case (slide, i) =>
      val pagination = s"${i + 1}/${presentation.slides.length}"
      Screen(
        List(
          Print(title, 1, (columns - title.length) / 2, None),
          Print(pagination, lines - 1, columns - pagination.length - 1, None)
        ) ++ buildBody(slide, 6, columns - 20)
      )
    }
  }

  private def buildBody(slide: presentation.Slide,
                        firstLine: Int,
                        columns: Int): List[Print] = {
    val maxColumns = Math.min(maxWidth(slide.body), columns)
    val firstColumn = (columns - maxColumns) / 2
    buildPrint(slide.body, firstLine, firstColumn + 10, maxColumns)._1
  }

  private def maxWidth(blocks: List[Block]): Int = blocks.foldLeft(0) { case (width, block) =>
    block match {
      //case Block.H1(inlines) =>
      //case Block.H2(inlines) =>
      case Par(inlines) => Math.max(width, maxWidthInlines(inlines))
      case ListItem(blocks) => Math.max(width, maxWidth(blocks))
    }
  }

  private def maxWidthInlines(inlines: List[Inline]): Int = {
    val (a, b) = inlines.foldLeft(0 -> 0) { case ((maxW, currW), inline) =>
      inline match {
        case Inline.Text(text) => maxW -> (currW + text.length)
        case Inline.Code(text) => maxW -> (currW + text.length)
        case Inline.NewLine => Math.max(maxW, currW) -> 0
      }
    }
    Math.max(a, b)
  }

  private def buildPrint(blocks: List[Block],
                         firstLine: Int,
                         firstColumn: Int,
                         maxColumns: Int): (List[Print], Int) =
    blocks.foldLeft(List.empty[Print] -> firstLine) { case ((prints, l), block) =>
      block match {
        //case Block.H1(inlines) =>
        //case Block.H2(inlines) =>
        case Par(inlines) =>
          val (p, (l2, _)) = buildPrintInline(inlines, l, firstColumn, maxColumns)
          (prints ++ p) -> (l2 + 1)
        case ListItem(blocks) =>
          val (p, l2) = buildPrint(blocks, l, firstColumn, maxColumns)
          val p2 = p.map(pr => pr.copy("- " + pr.text))
          (prints ++ p2) -> (l2 + 1)
      }
    }

  private def buildPrintInline(inlines: List[Inline],
                               firstLine: Int,
                               firstColumn: Int,
                               maxWidth: Int): (List[Print], (Int, Int)) =
    inlines.foldLeft(List.empty[Print] -> (firstLine -> firstColumn)) { case ((prints, (l, c)), inline) =>
      inline match {
        case Inline.Text(text) =>
          if ((text.length + c) > maxWidth) {
            val (before, after) = text.split(" ").foldLeft("" -> "") { case ((before, after), word) =>
              if (after.nonEmpty || (before ++ word).length > maxWidth) before -> s"$after $word"
              else s"$before $word" -> after
            }
            val p2 = Seq(
              Print(before, l, c, None),
              Print(after, l + 1, firstColumn, None)
            )
            (prints ++ p2) -> (l + 1 -> (firstColumn + after.length))
          }
          else (prints :+ Print(text, l, c, None)) -> (l -> (c + text.length))
        case Inline.Code(text) =>
          (prints :+ Print(text, l, c, None)) -> (l -> (c + text.length))
        case Inline.NewLine =>
          prints -> (l + 1 -> firstColumn)
      }

    }


  private def toText(inlines: List[Inline]): String = inlines.map {
    case Inline.Text(text) => text
    case Inline.Code(text) => text // FIXME invert color
    case Inline.NewLine => "\n"
  }.mkString(" ")
}
