package wntr10.adventofcode.y2023.d21

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}
import wntr10.adventofcode.{AocGrid, AocNode, AocStringValue, Input}


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def solve(input: Parser.Alpha): Int = {

    val grid = AocGrid.of(input, s => AocStringValue(s))

    val dirs = Set(LEFT, RIGHT, UP, DOWN)

    var current = grid.nodes.filter(n => n.value.str == "S")
    var last = Set.empty[AocNode]
    var steps = 0
    while (current.nonEmpty && steps < 64) {
      current = current.flatMap { n =>
        dirs.flatMap { d =>
          val r = grid.neighbor(n.key, d.dir, v => v.str == "." || v.str == "S")
          r
        }
      }
      current = current.diff(last)
      last  = current
      steps += 1
    }

    current.size
  }

  println(solve(input))

}
