package wntr10.adventofcode.y2023.d02

import wntr10.adventofcode.Input
import wntr10.adventofcode.y2023.d02.Parser.Alpha


object PartOne extends App {

  private lazy val input: Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def solve(alpha: Alpha): Int = {

    val config = Map(
      ("red" -> 12),
      ("green" -> 13),
      ("blue" -> 14)
    )

    alpha.filter { game =>
      game.b.forall { grab =>
        grab.forall { cube =>
          cube.a <= config(cube.b)
        }
      }
    }.map(m => m.a).sum

  }

  println(solve(input))

}
