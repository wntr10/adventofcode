package wntr10.adventofcode.y2023.d05

import wntr10.adventofcode.{AocRange, Input}


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def map(value: Long, ranges: List[(AocRange, AocRange)]): Long = {
    ranges.foreach { r =>
      if (r._1.contains(value)) {
        val idx = (value - r._1.start).toInt
        return r._2(idx)
      }
    }
    value
  }

  private def solve(input: Parser.Alpha): Long = {

    val seeds = input(0).b.toList.head.toList

    var list: List[Long] = seeds

    Range.inclusive(1, 7).foreach { step =>
      val ranges = input(step).b.map { l =>
        (AocRange.of(l(1), l(2)), AocRange.of(l(0), l(2)))
      }.toList
      list = list.map { p =>
        map(p, ranges)
      }
    }

    list.min

  }

  println(solve(input))

}
