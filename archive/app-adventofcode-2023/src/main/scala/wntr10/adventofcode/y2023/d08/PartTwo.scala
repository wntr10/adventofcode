package wntr10.adventofcode.y2023.d08

import wntr10.adventofcode.Input

object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private def next(x: Int, c: String, map: Map[String, (String, String)]): String = {
    val lr = map(c)
    val i = input.a.charAt(x).toString
    if (i == "R") {
      lr._2
    } else {
      lr._1
    }
  }

  private def factorize(x: Long, a: Long = 2): Set[Long] = {
    val o = x % a
    if (a * a > x) {
      Set(x)
    } else if (o == 0L) {
      Set(a) ++ factorize(x / a, a)
    } else {
      factorize(x, a + 1)
    }
  }

  private def solve(input: Parser.Alpha): Long = {

    val map = input.b.map { i =>
      i.a -> (i.b.a, i.b.b)
    }.toMap

    val cycles = map.keys.filter(_.endsWith("A")).map { label =>
      var steps = 0
      var current = label

      while (!current.endsWith("Z")) {
        val mo = steps % input.a.length
        current = next(mo, current, map)
        steps += 1
      }
      steps
    }

    cycles.flatMap(c => factorize(c.toLong)).product
  }

  println(solve(input))

}
