package wntr10.adventofcode.y2023.d09

import wntr10.adventofcode.Input


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def delta(l: List[Int]): List[Int] = {
    var listOfDeltas = List.empty[Int]

    l.foldLeft(Option.empty[Int])((prev, v) => {
      prev.foreach { p =>
        listOfDeltas = (v - p) :: listOfDeltas
      }
      Some(v)
    })

    listOfDeltas
  }

  private def solve(input: Parser.Alpha): Int = {

    input.map { line =>
      var stack = List(line.toList)
      while (!stack.head.forall(e => e == 0)) {
        stack = delta(stack.head).reverse :: stack
      }

      stack.foldLeft(0)((add, l) => l.head - add)

    }.sum

  }

  println(solve(input))

}
