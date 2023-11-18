package wntr10.adventofcode.y2022.d01

import wntr10.adventofcode.Input

object PartTwo extends App {

  private[this] lazy val input = {
    val input = new Input(this.getClass.getName, "")
    val lines = input.read
      .replace('\n', ';')
      .replace(";;", "~")

    lines
      .split('~').toList
      .map(r => r.split(';').toList)
  }

  private[this] def solve(input: List[List[String]]): Unit = {
    val p2 = input.map(r => r.map(_.toInt).sum)
    val total = p2.sorted.reverse.take(3).sum
    println(total.toString)
  }

  solve(input)

}
