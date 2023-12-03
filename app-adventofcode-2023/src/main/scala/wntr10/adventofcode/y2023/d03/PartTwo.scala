package wntr10.adventofcode.y2023.d03

import wntr10.adventofcode._

object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def next(grid: AocGrid, n: AocNode): (Option[AocNode], Set[AocKey]) = {
    val cont = grid.neighbor(n.key, 1, v => v.isNumber)
    val adjacent = grid.neighbors(n.key, v => v.str == "*").map(_.key).toSet
    (cont, adjacent)
  }

  private def solve(input: Parser.Alpha): Int = {
    val grid = AocGrid.of(input, s => AocStringValue(s))
    val subGrid = grid.filter(e => e.value.str != ".")

    var number = ""
    var parts = List.empty[(String, Set[AocKey])]
    var adj = Set.empty[AocKey]

    var done = Set.empty[AocKey]

    subGrid.foreach { p =>
      if (!done.contains(p.key)) {
        if (p.isNumber) {
          done = done + p.key
          number = number + p.value.str
          var n = next(subGrid, p)
          adj = adj ++ n._2

          while (n._1.isDefined) {
            done = done + n._1.get.key
            number = number + n._1.get.value.str
            n = next(subGrid, n._1.get)
            adj = adj ++ n._2
          }

          if (adj.nonEmpty) {
            parts = (number, adj) :: parts
          }
          number = ""
          adj = Set.empty
        }
      }
    }

    var gearMap = Map.empty[AocKey, Set[String]]
    parts.foreach { p =>
      p._2.foreach { g =>
        val f = gearMap.get(g)
        val prime = f.getOrElse(Set.empty) + p._1
        gearMap = gearMap.updated(g, prime)
      }
    }

    val gmf = gearMap.filter(_._2.size == 2)
    val ratio = gmf.map { g =>
      g._2.map(u => u.toInt).product
    }
    ratio.sum
  }

  println(solve(input))

}
