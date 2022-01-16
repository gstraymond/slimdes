package stuff

import cmark.EventType
import cmark.Iter
import cmark.Node
import cmark.NodeType

import scala.scalanative.unsafe._

package object event {

  sealed trait Event

  case class Enter(block: Block) extends Event
  case class Exit(block: Block) extends Event

  sealed trait Block

  object Block {
    case object H1 extends Block
    case object H2 extends Block
    case object Par extends Block
    case class Code(text: String) extends Block
    case object NewLine extends Block
    case object Item extends Block
    case object List extends Block
    case class Text(text: String) extends Block
  }

  def build(iter: Ptr[Iter])(evType: EventType): Option[Event] = {
    val node = Iter.getNode(iter)
    val maybeBlock = Node.getType(node) match {
      case NodeType.Text      => Some(Block.Text(fromCString(Node.getLiteral(node))))
      case NodeType.Code      => Some(Block.Code(fromCString(Node.getLiteral(node))))
      case NodeType.Paragraph => Some(Block.Par)
      case NodeType.SoftBreak => Some(Block.NewLine)
      case NodeType.List      => Some(Block.List)
      case NodeType.Item      => Some(Block.Item)
      case NodeType.Heading =>
        Node.getHeadingLevel(node) match {
          case 1 => Some(Block.H1)
          case 2 => Some(Block.H2)
          case l => println(s"skipped: H$l"); None
        }
      case NodeType.Document => None
      case n                 => println(s"skipped: $n"); None
    }

    maybeBlock.map { block =>
      evType match {
        case EventType.Enter => Enter(block)
        case _               => Exit(block)
      }
    }
  }

}
