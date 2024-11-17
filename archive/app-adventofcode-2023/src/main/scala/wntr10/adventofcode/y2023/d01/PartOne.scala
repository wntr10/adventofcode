package wntr10.adventofcode.y2023.d01

import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def digit(l: String): String = {
    l.find(c => c.isDigit).get.toString
  }

  private def solve(input: Parser.Alpha): Int = {

    input.map { l =>
      val fi = digit(l)
      val la = digit(l.reverse)
      (fi + la).toInt
    }.sum

  }

  println(solve(input))

}
