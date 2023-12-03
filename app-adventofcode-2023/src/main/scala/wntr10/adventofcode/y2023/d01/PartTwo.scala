package wntr10.adventofcode.y2023.d01

import wntr10.adventofcode.Input


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private val txt = List(
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
  ).zipWithIndex.map(d => (d._1, d._2 + 1))

  private val d = Range.inclusive(0, 9).map(n => (('0' + n).toChar.toString, n)).toList

  private val both = txt ::: d

  private def digit(l: String, dl: List[(String, Int)]): String = {
    dl.map(d => (l.indexOf(d._1), d._2)).filter(h => h._1 != -1).minBy(e => e._1)._2.toString
  }

  private def solve(input: Parser.Alpha): Int = {

    input.map { l =>
      val fi = digit(l, both)
      val la = digit(l.reverse, both.map(b => (b._1.reverse, b._2)))
      (fi + la).toInt
    }.sum

  }

  println(solve(input))

}
