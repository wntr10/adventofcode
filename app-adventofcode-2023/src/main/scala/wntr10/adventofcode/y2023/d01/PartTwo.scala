package wntr10.adventofcode.y2023.d01

import wntr10.adventofcode.Input


object PartTwo extends App {

  private lazy val input: List[String] = {
    val suffix = ""
    val input = new Input(this.getClass.getName, suffix)
    val lines = input.read
      .replace('\n', ';')

    lines.split(';').toList
  }

  private val digits = List(
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
  )

  private def digit(l: String, dl: List[String]): String = {
    val idxFirstString = (Integer.MAX_VALUE :: dl.map(d => l.indexOf(d)).filter(h => h != -1)).min
    val firstDigit = l.find(c => c.isDigit).get
    val idxFirstDigit = l.indexOf(firstDigit)
    val idxFirst = Math.min(idxFirstString, idxFirstDigit)
    val firstSubstring = l.substring(idxFirst)
    val firstStringOpt = dl.zipWithIndex.find(d => firstSubstring.startsWith(d._1))

    firstStringOpt.map(o => (o._2 + 1).toString).getOrElse(firstSubstring.head.toString)
  }

  private def solve(input: List[String]): Unit = {

    println(input.map { l =>
      val fi = digit(l, digits)
      val la = digit(l.reverse, digits.map(_.reverse))
      (fi + la).toInt
    }.sum)

  }

  solve(input)

}
