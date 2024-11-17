package wntr10.adventofcode.y2023.d04

import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def solve(input: Parser.Alpha): Int = {
    val pointList = input.map { g =>
      val winning = g.b.a.toSet
      val youHave = g.b.b

      var points = Option.empty[Int]

      youHave.filter(y => winning.contains(y)).foreach { _ =>
        points = if (points.isEmpty) {
          Some(1)
        } else {
          Some(points.get + points.get)
        }
        points
      }
      points.getOrElse(0)
    }

    pointList.sum
  }

  println(solve(input))

}
