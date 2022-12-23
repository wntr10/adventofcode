package wntr10.adventofcode.twenty.twenty.two

import org.junit.jupiter.api.Test
import wntr10.adventofcode.Input
import org.junit.jupiter.api.Assertions.assertEquals

class Day01Test {

  private lazy val input = {
    val in = new Input(this.getClass.getName)
    val row = in.rows
    var rowN = row.replace('\n', ';')
    rowN = rowN.replace(";;", "~")
    val inp = rowN
    val rows = inp.split('~').toList
    rows.map(r => r.split(';').toList)
  }

  @Test
  def testPartOne(): Unit = {
    //
  }

  @Test
  def testPartTwo(): Unit = {
    val day = new Day01(input)
    assertEquals("198551", day.partTwo())
  }

}
