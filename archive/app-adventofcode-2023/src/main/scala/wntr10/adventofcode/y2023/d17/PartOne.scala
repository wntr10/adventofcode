package wntr10.adventofcode.y2023.d17

import wntr10.adventofcode.AocDirection.{DOWN, LEFT, RIGHT, UP}
import wntr10.adventofcode._


object PartOne extends App {

  private lazy val input: Parser.Alpha = {
    val input = new Input(this.getClass.getName)
    val lines = input.read

    Parser.alpha(lines)
  }

  private case class SmallCrucible(n: AocNode, dir: Option[AocDirection], turnAfter: Int) extends Crucible

  private def start(): Set[AocDirection] = {
    Set(DOWN, UP, LEFT, RIGHT)
  }

  private def turn(dir: AocDirection): Set[AocDirection] = {
    dir match {
      case RIGHT => Set(DOWN, UP)
      case DOWN => Set(RIGHT, LEFT)
      case LEFT => Set(DOWN, UP)
      case UP => Set(RIGHT, LEFT)
      case _ => throw new RuntimeException()
    }
  }

  private def turnFree(dir: AocDirection): Set[AocDirection] = {
    dir match {
      case RIGHT => Set(DOWN, RIGHT, UP)
      case DOWN => Set(RIGHT, LEFT, DOWN)
      case LEFT => Set(DOWN, LEFT, UP)
      case UP => Set(RIGHT, LEFT, UP)
      case _ => throw new RuntimeException()
    }
  }

  private def route(grid: AocGrid)(next: (Crucible, (Crucible, BigInt))): Map[Crucible, BigInt] = {
    val b = next._1

    if (b.dir.isEmpty) {

      start().flatMap { nd =>
        val r = grid.neighbor(b.n.key, nd.dir, _ => true)
        r.map { fn =>
          SmallCrucible(fn, Some(nd), 2) -> (next._2._2 + BigInt(fn.value.str))
        }.toSet
      }.toMap

    } else if (b.turnAfter > 0) {

      turnFree(b.dir.get).flatMap { nd =>
        val r = grid.neighbor(b.n.key, nd.dir, _ => true)
        r.map { fn =>
          val fin = if (b.dir.get != nd) {
            2
          } else {
            b.turnAfter - 1
          }
          SmallCrucible(fn, Some(nd), fin) -> (next._2._2 + BigInt(fn.value.str))
        }.toSet
      }.toMap

    } else {

      turn(b.dir.get).flatMap { nd =>
        val r = grid.neighbor(b.n.key, nd.dir, _ => true)
        r.map { fn =>
          SmallCrucible(fn, Some(nd), 2) -> (next._2._2 + BigInt(fn.value.str))
        }.toSet
      }.toMap
    }
  }

  private def solve(input: Parser.Alpha): BigInt = {

    val grid = AocGrid.of(input, s => AocStringValue(s))

    val sb = SmallCrucible(grid.topLeft, None, 0)

    val dijkstra = new Dijkstra(sb)

    var stop = false

    while (!stop) {

      val next = dijkstra.next()
      if (next.isEmpty) {
        stop = true
      }

      next.foreach { next =>
        if (next._1.n == grid.bottomRight && next._1.keepDirectionFor == 0) {
          stop = true
        } else {
          dijkstra.update(next._1, route(grid)(next))
        }
      }
    }

    val heatLoss = dijkstra.sorted.closed.keys.filter(c => c.n == grid.bottomRight)
      .map(d => dijkstra.sorted.closed(d))
      .minBy(_._2)
      ._2

    heatLoss
  }

  println(solve(input))
}
