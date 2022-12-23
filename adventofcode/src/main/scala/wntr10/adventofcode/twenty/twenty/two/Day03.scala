package wntr10.adventofcode.twenty.twenty.two

import wntr10.adventofcode.Day

class Day03(input: List[String]) extends Day {

  private lazy val priorities = {
    val prio = (c: Char) => if (c.isLower) c.toInt - 96 else c.toInt - 38
    val toCharSet = (str: String) => str.toCharArray.toSet

    val prioritize = (list: List[String]) => {
      val i = list.foldLeft(Set.empty[Char]) {
        case (set, str) if set.isEmpty => toCharSet(str)
        case (set, str) => toCharSet(str).intersect(set)
      }
      require(i.size == 1)
      prio(i.head)
    }

    val p1 = input map { e =>
      prioritize(e.grouped(e.length / 2).toList)
    }

    val p2 = input.grouped(3).map(prioritize).sum
    (p1.sum, p2)
  }


  override def partOne(): String = {
    priorities._1.toString
  }

  override def partTwo(): String = {
    priorities._2.toString
  }
}
