package wntr10.adventofcode.y2023.d24

import wntr10.adventofcode.Input


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  case class Hailstone(px: BigDecimal,
                       py: BigDecimal,
                       pz: BigDecimal,
                       vx: BigDecimal,
                       vy: BigDecimal,
                       vz: BigDecimal) {

    val px2: BigDecimal = px + vx
    val py2: BigDecimal = py + vy

    def intersect(b: Hailstone): Option[(BigDecimal, BigDecimal)] = {
      val p0 = MyVector2D.of(px.toDouble, py.toDouble)
      val p1 = MyVector2D.of(px2.toDouble, py2.toDouble)
      val line = MyLine.fromPoints(p0, p1)

      val bp0 = MyVector2D.of(b.px.toDouble, b.py.toDouble)
      val bp1 = MyVector2D.of(b.px2.toDouble, b.py2.toDouble)
      val bline = MyLine.fromPoints(bp0, bp1)

      line.intersection(bline).flatMap { i =>
        val ray = MyLine.rayFromPointAndDirection(p0, line.direction)
        val bray = MyLine.rayFromPointAndDirection(bp0, bline.direction)
        if (ray.contains(i) && bray.contains(i)) {
          Some((i.x, i.y))
        } else {
          None
        }
      }
    }
  }


  private def solve(input: Parser.Alpha): Int = {
    val hs = input.map { l =>
      Hailstone(BigDecimal(l.a.head), BigDecimal(l.a(1)), BigDecimal(l.a.last), BigDecimal(l.b.head), BigDecimal(l.b(1)), BigDecimal(l.b.last))
    }

    val testMin: BigDecimal = 200000000000000L
    val testMax: BigDecimal = 400000000000000L

    var number = 0

    var done = Set.empty[(Hailstone, Hailstone)]

    hs.foreach { a =>
      hs.foreach { b =>
        if (a != b && !done.contains((b, a))) {
          done = done + ((a, b))
          val iopt = a.intersect(b)
          iopt.map { i =>
            if (i._1 >= testMin && i._1 <= testMax && i._2 >= testMin && i._2 <= testMax) {
              number += 1
            }
          }
        }
      }
    }

    number
  }

  println(solve(input))

}
