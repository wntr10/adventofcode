package wntr10.adventofcode.y2023.d00

import wntr10.adventofcode.Input


object PartOne extends App {

  //private val store = scala.collection.mutable.Map.empty[String, Int]

  private lazy val input: Parser.Alpha = {
    //val suffix = ""
    val suffix = ".ex0"
    val input = new Input(this.getClass.getName, suffix)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def solve(input: Parser.Alpha): Unit = {
    ()
  }

  println(solve(input))

}
