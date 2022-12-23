package wntr10.adventofcode.twenty.twenty.two

import wntr10.adventofcode.Day

class Day04(input: List[List[List[Int]]]) extends Day {

  private lazy val result = input.map { c =>
    val cp = c.map { p =>
      Set.range[Int](p.head, p.last + 1)
    }

    val ra = cp.head
    val rb = cp.last
    val p1 = if (ra.forall(e => rb.contains(e)) || rb.forall(e => ra.contains(e))) {
      1
    } else {
      0
    }
    val p2 = if (ra.exists(e => rb.contains(e)) || rb.exists(e => ra.contains(e))) {
      1
    } else {
      0
    }
    (p1, p2)
  }

  override def partOne(): String = {
    result.map(_._1).sum.toString
  }

  override def partTwo(): String = {
    result.map(_._2).sum.toString
  }
}
