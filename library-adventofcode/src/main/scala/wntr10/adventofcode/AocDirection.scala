package wntr10.adventofcode

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}

case class AocDirection(dir: Int) {
  //      4
  //  2 <-+-> 1
  //      3

  def horizontal: Boolean = dir == 1 || dir == 2

  def vertical: Boolean = dir == 3 || dir == 4

  def turn180(): AocDirection = dir match {
    case 1 => LEFT
    case 2 => RIGHT
    case 3 => UP
    case 4 => DOWN
  }

  def turnClockwise90(): AocDirection = dir match {
    case 1 => DOWN
    case 2 => UP
    case 3 => LEFT
    case 4 => RIGHT
  }

  def turnAntiClockwise90(): AocDirection = turnClockwise90().turn180()
}

object AocDirection {
  val LEFT: AocDirection = AocDirection(2)
  val WEST: AocDirection = LEFT

  val UP: AocDirection = AocDirection(4)
  val NORTH: AocDirection = UP

  val RIGHT: AocDirection = AocDirection(1)
  val EAST: AocDirection = RIGHT


  val DOWN: AocDirection = AocDirection(3)
  val SOUTH: AocDirection = DOWN

}
