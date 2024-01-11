package wntr10.adventofcode.y2023.d18

import wntr10.adventofcode.{AocGrid, AocStringValue, Input}

object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.parse(lines)
  }

  lazy val length: Int = input.length

  private case class Instr(dir: String, len: Int, color: String)

  private def solve(input: Parser.Alpha): Int = {

    val is = input.map(c => Instr(c.a, c.b, c.c))

    var set = Set.empty[(Int, Int)]
    var x = 0
    var y = 0

    is.foreach {
      case Instr("U", l, _) =>
        Range.inclusive(1, l).foreach { k =>
          y += 1
          val e: (Int, Int) = (x, y)
          set = set + e
        }
      case Instr("D", l, _) =>
        Range.inclusive(1, l).foreach { k =>
          y -= 1
          val e: (Int, Int) = (x, y)
          set = set + e
        }
      case Instr("L", l, _) =>
        Range.inclusive(1, l).foreach { k =>
          x -= 1
          val e: (Int, Int) = (x, y)
          set = set + e
        }
      case Instr("R", l, _) =>
        Range.inclusive(1, l).foreach { k =>
          x += 1
          val e: (Int, Int) = (x, y)
          set = set + e
        }
      case _ => throw new RuntimeException()
    }

    val prime = set.map(xy => (xy._1, xy._2, "#"))
    val grid = AocGrid.of(prime, s => AocStringValue(s))
    val gridPrime = AocGrid.of(AocGrid.pad(grid.arr("."), "."), s => AocStringValue(s))
    val flood = gridPrime.flood(gridPrime.topLeft, _ => v => v.str == ".")

    gridPrime.map.size - flood.map.size
  }

  println(solve(input))

}
