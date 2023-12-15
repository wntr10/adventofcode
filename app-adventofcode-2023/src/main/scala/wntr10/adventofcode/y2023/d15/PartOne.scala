package wntr10.adventofcode.y2023.d15

import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  def hash(s: String): Int = {
    var i = 0
    var value = 0
    while (i < s.length) {
      val char = s.charAt(i)
      val ascii = char.toByte.toInt
      value += ascii
      value = value * 17
      value = value % 256

      i += 1
    }
    value
  }

  private def solve(input: Parser.Alpha): Int = {
    input.map { e =>
      e.map { f =>
        hash(f)
      }.sum
    }.sum
  }

  println(solve(input))

}
