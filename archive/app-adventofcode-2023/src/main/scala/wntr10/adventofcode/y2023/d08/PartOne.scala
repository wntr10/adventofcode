package wntr10.adventofcode.y2023.d08

import com.google.common.collect.Iterators
import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def solve(input: Parser.Alpha): Int = {

    val it = Iterators.cycle(input.a: _*)

    val map = input.b.map { i =>
      i.a -> (i.b.a, i.b.b)
    }.toMap

    var steps = 0
    var current = "AAA"

    while (current != "ZZZ") {
      val lr = map(current)
      val i = it.next()

      current = if (i == 'R') {
        lr._2
      } else {
        lr._1
      }
      steps += 1
    }

    steps
  }

  println(solve(input))

}
