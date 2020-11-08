package foo

import stuff._
import stuff.block.Block._
import stuff.block.Inline._
import stuff.presentation.Error.{NoH1Found, TooManyH1Found}
import stuff.presentation.{Presentation, Slide}
import utest._

object MarkdownParserTests extends TestSuite {
  val tests = Tests {
    test("empty string") {
      parse() ==>
        Left(NoH1Found)
    }
    test("h1") {
      parse("# a title") ==>
        Right(Presentation(List(Text("a title")), Nil))
    }
    test("h1 h1") {
      parse("# h1", "# h1") ==>
        Left(TooManyH1Found)
    }
    test("h1 h2 h2") {
      parse("# h1", "## h2.1", "## h2.2") ==>
        Right(Presentation(List(Text("h1")), List(Slide(List(Text("h2.1")), Nil), Slide(List(Text("h2.2")), Nil))))
    }
    test("h1 + text + h2") {
      parse("# h1", "some text", "## h2") ==>
        Right(Presentation(List(Text("h1")), List(Slide(Nil, List(Par(List(Text("some text"))))), Slide(List(Text("h2")), Nil))))
    }
    test("h1 + code + text") {
      parse("# h1", "`code`", "text") ==>
        Right(Presentation(List(Text("h1")), List(Slide(Nil, List(Par(List(Code("code"), NewLine, Text("text"))))))))
    }
    test("h1 + list") {
      parse("# h1", "- a", "- b", "- c") ==>
        Right(Presentation(List(Text("h1")), List(Slide(Nil, List(ListItem(List(Par(List(Text("a"))), Par(List(Text("b"))), Par(List(Text("c"))))))))))
    }
  }

  private def parse(lines: String*) = Markdown.parse(lines.mkString("\n"))
}
