package wntr10.adventofcode.y2023.d16

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}
import wntr10.adventofcode._


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  lazy val length: Int = input.length

  private case class Beam(n: AocNode, dir: AocDirection)

  private def turn(beam: Beam): Set[Beam] = {
    (if (beam.dir.horizontal) "-" else "|", beam.dir, beam.n.value.str, beam.n) match {
      case ("-", _, "-", _) =>
        Set(beam)
      case ("-", _, "|", node) =>
        Set(Beam(node, UP), Beam(node, DOWN))

      case ("-", d, "l", node) =>
        Set(Beam(node, d.turnClockwise90()))
      case ("-", d, "/", node) =>
        Set(Beam(node, d.turnAntiClockwise90()))

      case ("|", _, "-", node) =>
        Set(Beam(node, LEFT), Beam(node, RIGHT))
      case ("|", _, "|", _) =>
        Set(beam)

      case ("|", d, "l", node) =>
        Set(Beam(node, d.turnAntiClockwise90()))
      case ("|", d, "/", node) =>
        Set(Beam(node, d.turnClockwise90()))

      case (_, _, ".", _) =>
        Set(beam)

      case _ => throw new RuntimeException()
    }
  }

  private def enter(start: Beam, grid: AocGrid): BigInt = {

    var store = Set(start)
    var energized = Set.empty[Beam]

    while (store.nonEmpty) {
      store = store.flatMap(turn).diff(energized)
      energized = energized ++ store

      store = store.flatMap { b =>
        grid.neighbor(b.n.key, b.dir.dir, _ => true).map(n => b.copy(n = n))
      }
    }

    energized.map(_.n).size
  }


  private def solve(input: Parser.Alpha): BigInt = {

    val grid = AocGrid.of(input, s => AocStringValue(s))
    enter(Beam(grid.topLeft, RIGHT), grid)

  }

  println(solve(input))
}
