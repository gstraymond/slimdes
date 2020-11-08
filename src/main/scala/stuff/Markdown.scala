package stuff

import cmark._
import stuff.presentation.{Error, Presentation}

import scala.scalanative.native
import scala.scalanative.native._

object Markdown {

  def parse(md: String): Either[Error, Presentation] = {
    native.Zone { implicit z =>

      val docNode = Parser.parseDocument(toCString(md), md.length, Options.Default)
      val iter = Iter.create(docNode)

      val events =
        Stream.continually(Iter.next(iter))
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

