import ncurses._
import ncursesh._
import stuff.Markdown
import stuff.screen

import scala.io.Source
import scala.scalanative.unsafe._

object Main {

  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("/Users/guillaume/git/csaw-intro-fp-scala/slides.md")
    val test = source.getLines.mkString("\n")

    Markdown
      .parse(test)
      .fold(
        println,
        { presentation =>
          val main = initscr()
          setCursorVisibility(CursorVisibility.Invisible)

          val size = windowSize(main)
          val screens = screen.build(presentation, size.height, size.width, hasColors())

          val (green, red, blue) = if (hasColors()) {
            startColor()
            useDefaultColors()
            val greenIndex = 1.toShort
            val redIndex = 2.toShort
            val blueIndex = 3.toShort
            initPair(greenIndex, foreground = Color.Green, background = Color.Transparent)
            initPair(redIndex, foreground = Color.Red, background = Color.Transparent)
            initPair(blueIndex, foreground = Color.Blue, background = Color.Transparent)

            (Some(colorPair(greenIndex)), Some(colorPair(redIndex)), Some(colorPair(blueIndex)))
          } else (None, None, None)

          screens.foreach { screen =>
            eraseWindow(main)

            //box(main, 0, 0)
            Zone { implicit z =>
              screen.prints.foreach { print =>
                displayText(print.text, print.line, print.column, print.attribute.map(new Attribute(_)))
              }

//            val maxBodyWidth = size.width - (2 * 10)
//
//            def centerText(text: String, line: Int, color: Option[Attribute]) =
//              displayText(text, line, (size.width - text.length) / 2, color)
//
              def displayText(text: String, line: Int, column: Int, color: Option[Attribute]): Unit = {
                color.foreach(attributeOn(main, _))
                printFormatted(main, line, column, c"%s", toCString(text))
                color.foreach(attributeOff(main, _))
              }

//            centerText(presentation.title, 2, green)
//            val bodyLines = sli de.body.flatMap {
//              case line if line.length > maxBodyWidth => line.grouped(maxBodyWidth)
//              case line => List(line)
//            }
//
//            // FIXME handle bodyLines empty
//            val bodyWidth = bodyLines.map(_.length).max
//            val margin = (size.width - bodyWidth) / 2
//
//            centerText(slide.title, 4, blue)
//            bodyLines.zipWithIndex.foreach { case (line, i) =>
//              displayText(line, 6 + i, margin, None)
//            }

            //val position = s"${i + 1} / ${presentation.slides.length}"
            //displayText(position, size.height - 2, size.width - position.length - 1, red)
            }
            refresh()
            getch()
          }
          endwin()
        }
      )
  }
}
