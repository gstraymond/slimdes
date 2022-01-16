package foo

import stuff.Markdown
import stuff.screen
import stuff.screen.Print
import stuff.screen.Screen
import utest._

object ScreenBuilderTests extends TestSuite {
  val tests = Tests {
    test("h1 and text") {
      parse(80, 80, "# h1", "text") ==>
        List(
          Screen(
            List(
              Print("h1", 1, 39, None),
              Print("1/1", 79, 76, None),
              Print("text", 6, 38, None)
            )
          )
        )
    }
    test("h1 and list") {
      parse(80, 80, "# h1", "## slide 1", "- a", "- b", "## slide 2", "some `code`", "text") ==>
        List(
          Screen(
            List(
              Print("h1", 1, 39, None),
              Print("1/2", 79, 76, None),
              Print("- a", 6, 39, None),
              Print("- b", 7, 39, None)
            )
          ),
          Screen(
            List(
              Print("h1", 1, 39, None),
              Print("2/2", 79, 76, None),
              Print("some ", 6, 35, None),
              Print("code", 6, 40, None),
              Print("text", 7, 35, None)
            )
          )
        )
    }
  }

  private def parse(l: Int, c: Int, lines: String*) =
    Markdown
      .parse(lines.mkString("\n"))
      .fold(
        _ => List.empty[Screen],
        screen.build(_, l, c, hasColors = false)
      )

}
