package wntr10.adventofcode.y2023.d23

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}
import wntr10.adventofcode.{AocGrid, AocStringValue, Input}


object PartTwo extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length


  private def solve(input: Parser.Alpha): Unit = {

    // I added S and T to the input file

    val grid = AocGrid.of(input, s => AocStringValue(s))

    val dirs = List(DOWN, RIGHT, LEFT, UP)

    val start = grid.nodes.find(n => n.value.str == "S").get
    val target = grid.nodes.find(n => n.value.str == "T").get

    var current = List((start, Set(start)))
    var stop = false

    var max = 0
    while (!stop) {

      val h = current.head
      val rest = current.drop(1)

      val next = dirs.flatMap { dir =>
        val r = grid.neighbor(h._1.key, dir.dir, v => "T^>v<.".contains(v.str))
        if (r.isDefined && h._2.contains(r.get)) {
          None
        } else {
          r
        }
      }

      val nl = next.map { n =>
        val ll = (n, h._2 + n)
        if (n == target) {
          max = Math.max(ll._2.size, max)
          println("potential max = " + (max - 1))
        }
        ll
      }

      current = nl ::: rest

      if (current.isEmpty) {
        stop = true
      }
    }
  }

  println(solve(input))

}
