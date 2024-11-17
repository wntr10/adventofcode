package wntr10.adventofcode.y2023.d19

import wntr10.adventofcode.Input

import scala.annotation.tailrec


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  private case class Workflow(rules: List[String]) {

    def receive(part: Map[String, Int], ws: Map[String, Workflow]): Boolean = {
      receiveInt(rules, part, ws)
    }

    @tailrec
    private def receiveInt(rs: List[String], part: Map[String, Int], ws: Map[String, Workflow]): Boolean = {
      val r = rs.head
      r match {
        case s"$p<$v:A" =>
          if (part(p) < v.toInt) return true
        case s"$p>$v:A" =>
          if (part(p) > v.toInt) return true
        case s"$p<$v:R" =>
          if (part(p) < v.toInt) return false
        case s"$p>$v:R" =>
          if (part(p) > v.toInt) return false
        case s"$p>$v:$w" =>
          if (part(p) > v.toInt) return ws(w).receive(part, ws)
        case s"$p<$v:$w" =>
          if (part(p) < v.toInt) return ws(w).receive(part, ws)
        case "A" => return true
        case "R" => return false
        case w => return ws(w).receive(part, ws)
      }
      receiveInt(rs.drop(1), part, ws)
    }
  }

  private def solve(input: Parser.Alpha): Int = {

    val parts = input.b.map { p =>
      p.map(e => e.a -> e.b).toMap
    }

    val workflows = input.a.map { w =>
      w.a -> Workflow(w.b.toList)
    }.toMap

    parts.map { p =>
      if (workflows("in").receive(p, workflows)) {
        p.values.sum
      } else {
        0
      }
    }.sum

  }

  println(solve(input))

}
