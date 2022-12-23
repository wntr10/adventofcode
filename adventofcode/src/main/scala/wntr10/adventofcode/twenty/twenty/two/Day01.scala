package wntr10.adventofcode.twenty.twenty.two

import wntr10.adventofcode.Day

final class Day01(input: List[List[String]]) extends Day {

  override def partOne(): String = {
    ""
  }

  override def partTwo(): String = {
    val p2 = input.map(r => r.map(_.toInt).sum)
    val total = p2.sorted.reverse.take(3).sum
    total.toString
  }
}
