package wntr10.adventofcode.y2023.d03

import wntr10.adventofcode._

object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def next(grid: AocGrid, n: AocNode): (Option[AocNode], Boolean) = {
    val cont = grid.neighbor(n.key, 1, v => v.isNumber)
    val adjacent = grid.neighbors(n.key, v => !v.isNumber).nonEmpty
    (cont, adjacent)
  }

  private def solve(input: Parser.Alpha): Int = {
    val grid = AocGrid.of(input, s => AocStringValue(s))
    val subGrid = grid.filter(e => e.value.str != ".")

    var number = ""
    var parts = List.empty[String]
    var adj = false

    var done = Set.empty[AocKey]

    subGrid.foreach { p =>
      if (!done.contains(p.key)) {
        if (p.isNumber) {
          done = done + p.key
          number = number + p.value.str
          var n = next(subGrid, p)
          adj = adj | n._2

          while (n._1.isDefined) {
            done = done + n._1.get.key
            number = number + n._1.get.value.str
            n = next(subGrid, n._1.get)
            adj = adj | n._2
          }

          if (adj) {
            parts = number :: parts
          }
          number = ""
          adj = false
        }
      }
    }

    parts.map(_.toInt).sum
  }

  println(solve(input))

}
