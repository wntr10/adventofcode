package wntr10.adventofcode.twenty.twenty.two

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import wntr10.adventofcode.Input

class Day23Test {

  private lazy val input = {
    val input = new Input(this.getClass.getName)
    val rawN = input.rows.trim.replace("\n", ";")
    rawN.split(';').toList
  }

  private lazy val day = new Day23(input)

  @Test
  def testPartOne(): Unit = {
    //
  }

  @Test
  def testPartTwo(): Unit = {
    assertEquals("990", day.partTwo())
  }
}
