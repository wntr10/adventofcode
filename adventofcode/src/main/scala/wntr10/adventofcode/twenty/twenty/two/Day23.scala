package wntr10.adventofcode.twenty.twenty.two

import wntr10.adventofcode.Day

class Day23(input: List[String]) extends Day {

  private case class Point(x: Int, y: Int) {

    lazy val w: Set[Point] = Set(
      Point(x - 1, y),
      Point(x - 1, y - 1),
      Point(x - 1, y + 1))

    lazy val n: Set[Point] = Set(
      Point(x, y - 1),
      Point(x - 1, y - 1),
      Point(x + 1, y - 1))

    lazy val o: Set[Point] = Set(
      Point(x + 1, y),
      Point(x + 1, y + 1),
      Point(x + 1, y - 1))

    lazy val s: Set[Point] = Set(
      Point(x, y + 1),
      Point(x + 1, y + 1),
      Point(x - 1, y + 1))

    lazy val all: Set[Point] = w ++ n ++ o ++ s
  }

  private val map = scala.collection.mutable.Map.empty[Point, Boolean]

  override def partOne(): String = {
    ""
  }

  override def partTwo(): String = {
    input.zipWithIndex.foreach { l =>
      l._1.toCharArray.map(_.toString).zipWithIndex.map { p =>
        if (p._1 == "#") {
          map.put(Point(p._2, l._2), true)
        }
      }
    }

    var check = List(
      ((p: Point) => p.n, (p: Point) => Point(p.x, p.y - 1)),
      ((p: Point) => p.s, (p: Point) => Point(p.x, p.y + 1)),
      ((p: Point) => p.w, (p: Point) => Point(p.x - 1, p.y)),
      ((p: Point) => p.o, (p: Point) => Point(p.x + 1, p.y)))

    var ms = map.keySet

    for (i <- 0 until 100000) {

      var list = List.empty[(Point, Point)]
      var stay = List.empty[Point]

      ms.foreach { e =>
        val hasN = e.all.exists { e => ms.contains(e) }
        if (hasN) {
          val c = check.find(s => s._1(e).forall(sp => !ms.contains(sp)))
          if (c.isDefined) {
            list = (c.get._2(e), e) :: list
          } else {
            stay = e :: stay
          }
        } else {
          stay = e :: stay
        }
      }

      val mem = scala.collection.mutable.Map.empty[Point, (Point, Point)]
      val remove = scala.collection.mutable.Set.empty[(Point, Point)]

      list.foreach { e =>
        if (mem.keySet.contains(e._1)) {
          remove.add(e)
          remove.add(mem(e._1))
        }
        mem.put(e._1, e)
      }

      val prime = list.toSet -- remove
      val move = prime.map(_._1)
      if (move.isEmpty) {
        return (i + 1).toString
      }
      ms = move ++ remove.map(_._2) ++ stay

      val h = check.head
      check = check.drop(1) :+ h
    }
    "should not happen"
  }
}
