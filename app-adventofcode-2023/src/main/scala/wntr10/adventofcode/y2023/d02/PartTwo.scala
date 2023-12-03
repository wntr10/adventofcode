package wntr10.adventofcode.y2023.d02

import wntr10.adventofcode.Input
import wntr10.adventofcode.y2023.d02.Parser.Alpha


object PartTwo extends App {

  private lazy val input: Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def solve(alpha: Alpha): Int = {

    val colors = List(
      "red",
      "green",
      "blue"
    )

    alpha.map { game =>
      colors.map { c =>
        game.b.flatMap { grab =>
          grab.filter { cube =>
            cube.b == c
          }
        }.map(cube => cube.a).max
      }.product
    }.sum

  }

  println(solve(input))

}
