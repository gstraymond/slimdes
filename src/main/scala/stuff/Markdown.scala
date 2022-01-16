package stuff

import cmark._
import stuff.presentation.Error
import stuff.presentation.Presentation

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

object Markdown {

  def parse(md: String): Either[Error, Presentation] = {
    Zone { implicit z =>
      val docNode = Parser.parseDocument(toCString(md), md.length().toULong, Options.Default)
      val iter = Iter.create(docNode)

      val events =
        Stream
          .continually(Iter.next(iter))
          .takeWhile(_ != EventType.Done)
          .flatMap(event.build(iter))

      val blocks = block.build(events)
      println(s"blocks: [${blocks.mkString(", ")}]")

      val presentationOrError = presentation.build(blocks)

      Iter.free(iter)
      Node.free(docNode)

      presentationOrError
    }
  }
}
