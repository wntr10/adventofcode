package wntr10.adventofcode.twenty.twenty.two

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import wntr10.adventofcode.Input

class Day02Test {

  private lazy val input = {
    val input = new Input(this.getClass.getName)
    var rawN = input.rows
    rawN = rawN.replace('\n', ';')
    rawN = rawN.replace(" ", ",")
    val inp = rawN

    val rows = inp.split(';').toList
    rows.map { row =>
      val columns = row.split(',').toList
      require(columns.size == 2)
      columns
    }
  }

  private lazy val day = new Day02(input)

  @Test
  def testPartOne(): Unit = {
    assertEquals("13446", day.partOne())
  }

  @Test
  def testPartTwo(): Unit = {
    assertEquals("13509", day.partTwo())
  }
}
