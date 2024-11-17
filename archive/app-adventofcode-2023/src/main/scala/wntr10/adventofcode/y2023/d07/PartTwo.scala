package wntr10.adventofcode.y2023.d07

import wntr10.adventofcode.Input
import wntr10.adventofcode.y2023.d07.Parser.Bravo


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  val labels = List("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J").reverse

  def kind(cardWithJ: String): Int = {
    labels.filter(l => l != "J").map { l =>
      val card = cardWithJ.replace('J', l.head)
      val set = card.map(_.toString).toSet
      val map = set.map { c =>
        c -> card.count(p => p == c.head)
      }.toMap
      val max = map.maxBy(_._2)
      if (set.size == 1) {
        7
      } else if (max._2 == 4) {
        6
      } else if (set.size == 2 && max._2 == 3) {
        5
      } else if (set.size == 3 && max._2 == 3) {
        4
      } else if (set.size == 3 && max._2 == 2) {
        3
      } else if (set.size == 4 && max._2 == 2) {
        2
      } else {
        1
      }
    }.max
  }

  private def second(x: String, y: String): Int = {
    x.indices.foreach { i =>
      val sx = labels.indexOf(x(i).toString)
      val sy = labels.indexOf(y(i).toString)
      val c = sx.compareTo(sy)
      if (c != 0) {
        return c
      }
    }
    0
  }

  private def solve(input: Parser.Alpha): Int = {

    val ordering = new Ordering[Bravo] {
      override def compare(x: Bravo, y: Bravo): Int = {
        val c = kind(x.a).compareTo(kind(y.a))
        if (c == 0) {
          second(x.a, y.a)
        } else {
          c
        }
      }
    }

    input.sorted(ordering).toList.zipWithIndex.map { c =>
      (c._2 + 1) * c._1.b
    }.sum
  }

  println(solve(input))

}
