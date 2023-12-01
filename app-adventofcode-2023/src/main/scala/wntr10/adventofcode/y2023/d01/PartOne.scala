package wntr10.adventofcode.y2023.d01

import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: List[String] = {
    val suffix = ""
    val input = new Input(this.getClass.getName, suffix)
    val lines = input.read
      .replace('\n', ';')

    lines.split(';').toList
  }

  private def digit(l: String): String = {
    l.find(c => c.isDigit).get.toString
  }

  private def solve(input: List[String]): Unit = {

    println(input.map { l =>
      val fi = digit(l)
      val la = digit(l.reverse)
      (fi + la).toInt
    }.sum)

  }

  solve(input)

}
