package wntr10.adventofcode.y2023.d23

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}
import wntr10.adventofcode.{AocGrid, AocNode, AocStringValue, Input}


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private def solve(input: Parser.Alpha): Int = {

    // I added S and T to the input file

    val grid = AocGrid.of(input, s => AocStringValue(s))

    val dirs = Set(DOWN, UP, LEFT, RIGHT)

    val start = grid.nodes.find(n => n.value.str == "S").get

    var current = Set(List(start))
    var last = Set.empty[List[AocNode]]
    var stop = false
    while (!stop) {

      val prime = current.flatMap { cl =>
        val c = cl.head

        val turns = c.value.str match {
          case "^" => Set(UP)
          case "v" => Set(DOWN)
          case ">" => Set(RIGHT)
          case "<" => Set(LEFT)
          case _ => dirs
        }
        val next = turns.flatMap { dir =>
          val r = grid.neighbor(c.key, dir.dir, v => "T^>v<.".contains(v.str))
          if (r.isDefined && cl.contains(r.get)) {
            None
          } else {
            r
          }
        }
        next.map(n => n :: cl)
      }
      last = current
      current = prime
      if (current.isEmpty) {
        stop = true
      }
    }

    val goal = last.filter(l => l.head.value.str == "T").maxBy(_.length)

    goal.length - 1
  }

  println(solve(input))

}
