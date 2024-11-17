package wntr10.adventofcode.y2023.d14

import wntr10.adventofcode._


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def solve(input: Parser.Alpha): BigInt = {

    val grid = AocGrid.of(input, s => AocStringValue(s))

    var gridPrime = grid
    var rocks = List.empty[AocNode]

    var stop = false
    while (!stop) {
      stop = true
      rocks = gridPrime.nodes.filter(n => n.value.str == "O").toList
      rocks.foreach { r =>
        val ng = gridPrime.move(r, 4, v => v.str == ".", AocStringValue("."))
        if (ng.isDefined) {
          gridPrime = ng.get
          stop = false
        }
      }
    }

    rocks = gridPrime.nodes.filter(n => n.value.str == "O").toList

    rocks.map { r =>
      val xy = AocInterleave.squash(BigInt(r.key.str, 4))
      xy._2 + 1
    }.sum

  }

  println(solve(input))
}
