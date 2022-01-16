package stuff

import stuff.event.Enter
import stuff.event.Event
import stuff.event.Exit

package object block {

  sealed trait Block

  object Block {

    case class H1(inlines: List[Inline]) extends Block

    case class H2(inlines: List[Inline]) extends Block

    case class Par(inlines: List[Inline]) extends Block

    case class ListItem(blocks: List[Block]) extends Block

  }

  sealed trait Inline

  object Inline {

    case class Text(text: String) extends Inline

    case class Code(text: String) extends Inline

    case object NewLine extends Inline

  }

  def build(events: Stream[Event]): List[Block] =
    events
      .foldLeft(List.empty[Block] -> List.empty[Inline]) { case ((blocks, inlines), current) =>
        current match {
          case Enter(event.Block.Text(text)) => blocks -> (inlines :+ Inline.Text(text))
          case Enter(event.Block.Code(text)) => blocks -> (inlines :+ Inline.Code(text))
          case Enter(event.Block.NewLine)    => blocks -> (inlines :+ Inline.NewLine)
          case Enter(event.Block.List)       => (blocks :+ Block.ListItem(Nil)) -> inlines
          case Exit(event.Block.H1)          => (blocks :+ Block.H1(inlines)) -> Nil
          case Exit(event.Block.H2)          => (blocks :+ Block.H2(inlines)) -> Nil
          case Exit(event.Block.Par)         => (blocks :+ Block.Par(inlines)) -> Nil
          case Exit(event.Block.List) =>
            val (before, after) = blocks.splitAt(blocks.indexOf(Block.ListItem(Nil)))
            (before :+ Block.ListItem(after.drop(1))) -> Nil
          case any => println(s"skipped: $any"); blocks -> inlines
        }
      }
      ._1
}
