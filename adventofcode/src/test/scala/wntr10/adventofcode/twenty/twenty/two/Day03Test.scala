package wntr10.adventofcode.twenty.twenty.two

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import wntr10.adventofcode.Input

class Day03Test {

  private lazy val input = {
    val input = new Input(this.getClass.getName)
    var rawN = input.rows
    rawN = rawN.replace('\n', ';')
    val inp = rawN
    inp.split(';').toList
  }

  private lazy val day = new Day03(input)

  @Test
  def testPartOne(): Unit = {
    assertEquals("7553", day.partOne())
  }

  @Test
  def testPartTwo(): Unit = {
    assertEquals("2758", day.partTwo())
  }
}
